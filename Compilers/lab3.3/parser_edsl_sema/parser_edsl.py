import abc
import collections
import dataclasses
import regex as re
import sys


__all__ = '''
Terminal
ExAction
NonTerminal
EOF_SYMBOL
Position
Fragment
Parser
Error
'''.split()


class Symbol:
    pass


class BaseTerminal(Symbol):
    pass


class Terminal(BaseTerminal):
    def __init__(self, name, regex, func, *, priority=5, re_flags=re.MULTILINE):
        self.name = name
        self.regex = regex
        self.func = func
        self.priority = priority
        self.re = re.compile(regex, re_flags)

    def __repr__(self):
        return f'Terminal({self.name!r},{self.regex!r},{self.func!r})'

    def __str__(self):
        return self.name

    def match(self, string, pos):
        m = self.re.match(string, pos)
        if m != None:
            begin, end = m.span()
            attrib = self.func(string[begin:end])
            return end - begin, attrib
        else:
            return 0, None


class LiteralTerminal(BaseTerminal):
    def __init__(self, image):
        self.image = image
        self.priority = 10

    def __hash__(self):
        return hash(self.image)

    def __eq__(self, other):
        return type(self) == type(other) and self.image == other.image

    def __repr__(self):
        return f'LiteralTerminal({self.image!r})'

    def __str__(self):
        return repr(self.image)

    def match(self, string, pos):
        if string.startswith(self.image, pos):
            return len(self.image), None
        else:
            return 0, None


class SpecTerminal(BaseTerminal):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f'SpecTerminal({self.name})'

    def __str__(self):
        return self.name


EOF_SYMBOL = SpecTerminal('EOF')
FREE_SYMBOL = SpecTerminal('#')


class ErrorTerminal(BaseTerminal):
    priority = -1

    @staticmethod
    def match(string, pos):
        assert pos < len(string)
        return 1, ErrorTerminal


@dataclasses.dataclass(frozen = True)
class ExAction():
    callee : object

    @staticmethod
    def wrap_simple_action(simple_fold):
        def extended_action(attrs, coords, res_coord):
            return simple_fold(*attrs)

        return ExAction(extended_action)


class NonTerminal(Symbol):
    def __init__(self, name):
        self.name = name
        self.productions = []
        self.lambdas = []

    def __repr__(self):
        return 'NonTerminal(' + repr(self.name) + ')'

    def __str__(self):
        return self.name

    def stringify(self, pretty=True):
        title = '%s: ' % self.name

        if pretty:
            separator = '\n%s| ' % (' ' * len(self.name))
        else:
            separator = ''

        def strprod(prod):
            return ' '.join(str(sym) for sym in prod)

        rules = separator.join(strprod(prod) for prod in self.productions)
        return title + rules


    @staticmethod
    def __wrap_literals(symbol):
        if isinstance(symbol, str):
            return LiteralTerminal(symbol)
        else:
            assert isinstance(symbol, Symbol)
            return symbol


    def __ior__(self, other):
        is_callable = lambda obj: hasattr(obj, '__call__')
        is_fold = lambda obj: is_callable(obj) or isinstance(obj, ExAction)

        if other == ():
            self |= lambda: None
        elif isinstance(other, tuple) and isinstance(other[-1], ExAction):
            *symbols, fold = other
            symbols = [self.__wrap_literals(sym) for sym in symbols]
            self.productions.append(symbols)
            self.lambdas.append(fold)
        elif isinstance(other, tuple) and is_callable(other[-1]):
            self |= other[:-1] + (ExAction.wrap_simple_action(other[-1]),)
        elif isinstance(other, tuple):
            self |= other + (self.__default_fold,)
        elif isinstance(other, Symbol) or is_fold(other):
            self |= (other,)
        elif isinstance(other, str):
            self |= (LiteralTerminal(other),)
        else:
            raise Exception('Bad rule')

        return self

    @staticmethod
    def __default_fold(*args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 0:
            return None
        else:
            raise RuntimeError('__default_fold', args)

    def enum_rules(self):
        return zip(self.productions, self.lambdas)


@dataclasses.dataclass(frozen = True)
class Position:
    offset : int = 0
    line : int = 1
    col : int = 1

    def shift(self, text : str):
        offset, line, col = dataclasses.astuple(self)

        for char in text:
            if char == '\n':
                line += 1
                col = 1
            else:
                col += 1

        return Position(offset + len(text), line, col)

    def __str__(self):
        return f'({self.line}, {self.col})'


@dataclasses.dataclass(frozen = True)
class Fragment:
    start : Position
    following : Position

    def __str__(self):
        return f'{self.start}-{self.following}'


@dataclasses.dataclass
class Token:
    type : BaseTerminal
    pos : Fragment
    attr : object

    def __str__(self):
        if self.attr is not None:
            return f'{self.type}({self.attr})'
        else:
            return str(self.type)


class LrZeroItemTableEntry:
    def __init__(self):
        self.propagates_to = set()
        self.lookaheads = set()

    def __repr__(self):
        pattern = '{ propagatesTo: %s, lookaheads: %s }'
        return pattern % (repr(self.propagates_to), repr(self.lookaheads))


Shift = collections.namedtuple('Shift', 'state')
Reduce = collections.namedtuple('Reduce', 'rule')
Accept = collections.namedtuple('Accept', '')


class ParsingTable:
    def __init__(self, gr):
        self.grammar = gr

        self.terminals = ()
        self.nonterms = ()
        self.__ccol = ()
        self.n_states = 0

        self.goto = ()
        self.action = ()

        self.__setup_from_grammar(self.grammar)

    def __setup_from_grammar(self, gr):
        self.terminals = gr.terminals + tuple([EOF_SYMBOL])
        self.nonterms = gr.nonterms[1:]

        self.__ccol = tuple(get_canonical_collection(gr))
        self.n_states = len(self.__ccol)

        ccol_core = tuple(drop_itemset_lookaheads(x) for x in self.__ccol)
        id_from_core = {ccol_core[i]: i for i in range(len(self.__ccol))}

        self.goto = tuple({x: None for x in self.nonterms} for i in range(self.n_states))
        self.action = tuple({x: set() for x in self.terminals} for i in range(self.n_states))

        goto_precalc = tuple(dict() for i in range(self.n_states))
        for symbol in (self.terminals + self.nonterms):
            for state_id in range(self.n_states):
                next_state = goto(gr, self.__ccol[state_id], symbol)
                if len(next_state) == 0:
                    continue
                next_state_id = id_from_core[drop_itemset_lookaheads(next_state)]
                goto_precalc[state_id][symbol] = next_state_id

        for state_id in range(self.n_states):
            for item, next_symbol in self.__ccol[state_id]:
                prod_index, dot = item
                pname, pbody, plambda = gr.productions[prod_index]

                if dot < len(pbody):
                    terminal = pbody[dot]
                    if not isinstance(terminal, BaseTerminal) or terminal not in goto_precalc[state_id]:
                        continue

                    next_state_id = goto_precalc[state_id][terminal]
                    self.action[state_id][terminal].add(Shift(next_state_id))
                else:
                    if prod_index == 0:
                        assert (next_symbol == EOF_SYMBOL)
                        self.action[state_id][EOF_SYMBOL].add(Accept())
                    else:
                        self.action[state_id][next_symbol].add(Reduce(prod_index))

            for nt in self.nonterms:
                if nt not in goto_precalc[state_id]:
                    continue
                next_state_id = goto_precalc[state_id][nt]
                self.goto[state_id][nt] = next_state_id

    @staticmethod
    def __stringify_action_entries(term, ent):
        return '\tfor terminal %s: ' % term + ', '.join(map(str, ent))

    @staticmethod
    def __stringify_goto_entry(nt, sid):
        return '\tfor non-terminal %s: go to state %d' % (str(nt), sid)

    def __stringify_lr_zero_item(self, item):
        prod_index, dot = item
        pname, pbody, plambda = self.grammar.productions[prod_index]
        dotted_pbody = pbody[:dot] + ['.'] + pbody[dot:]
        dotted_pbody_str = ' '.join(str(x) for x in dotted_pbody)
        return RULE_INDEXING_PATTERN % (prod_index, pname.name + ': ' + dotted_pbody_str)

    def stringify_state(self, state_id):
        state_title = 'State %d\n' % state_id
        items = drop_itemset_lookaheads(kernels(self.__ccol[state_id]))
        items = sorted(items, key=lambda elem: elem[0])
        items_str = '\n'.join('\t' + self.__stringify_lr_zero_item(item) for item in items) + '\n\n'
        # TODO CHANGED FOR TERMINALS MAYBE WRONG
        actions = [(t, e) for t, e in self.action[state_id].items() if len(e) > 0]
        actions_str = '\n'.join(self.__stringify_action_entries(t, e) for t, e in actions)
        actions_str += ('\n' if len(actions_str) > 0 else '')

        gotos = [(nt, sid) for nt, sid in self.goto[state_id].items() if sid is not None]
        gotos = sorted(gotos, key=lambda elem: elem[0].name)

        gotos_str = '\n'.join(self.__stringify_goto_entry(nt, sid) for nt, sid in gotos)
        gotos_str += ('\n' if len(gotos_str) > 0 else '')

        action_goto_separator = ('\n' if len(actions_str) > 0 and len(gotos_str) > 0 else '')
        return state_title + items_str + actions_str + action_goto_separator + gotos_str

    def stringify(self):
        states_str = '\n'.join(self.stringify_state(i) for i in range(self.n_states))
        return states_str

    @staticmethod
    def __get_entry_status(e):
        if len(e) <= 1:
            return STATUS_OK
        n_actions = len(frozenset(type(a) for a in e))
        return STATUS_SR_CONFLICT if n_actions == 2 else STATUS_RR_CONFLICT

    def get_single_state_conflict_status(self, state_id):
        seq = [self.__get_entry_status(e) for t, e in self.action[state_id].items()]
        return STATUS_OK if len(seq) == 0 else max(seq)

    def get_conflict_status(self):
        return [self.get_single_state_conflict_status(i) for i in range(self.n_states)]

    def is_lalr_one(self):
        seq = self.get_conflict_status()
        return (STATUS_OK if len(seq) == 0 else max(seq)) == STATUS_OK


def get_canonical_collection(gr):
    dfa = LR0_Automaton(gr)
    kstates = dfa.kstates()
    n_states = len(kstates)

    table = [{item: LrZeroItemTableEntry() for item in kstates[i]} for i in range(n_states)]
    table[0][(0, 0)].lookaheads.add(EOF_SYMBOL)

    for i_state_id in range(n_states):
        state_symbols = [x[1] for x, y in dfa.goto.items() if x[0] == i_state_id]

        for i_item in kstates[i_state_id]:
            closure_set = closure(gr, [(i_item, FREE_SYMBOL)])

            for sym in state_symbols:
                j_state_id = dfa.goto[(i_state_id, sym)]

                # For each item in closure_set whose . (dot) points to a symbol equal to 'sym'
                # i.e. a production expecting to see 'sym' next
                for ((prod_index, dot), next_symbol) in closure_set:
                    pname, pbody, plambda = gr.productions[prod_index]
                    if dot == len(pbody) or pbody[dot] != sym:
                        continue

                    j_item = (prod_index, dot + 1)
                    if next_symbol == FREE_SYMBOL:
                        table[i_state_id][i_item].propagates_to.add((j_state_id, j_item))
                    else:
                        table[j_state_id][j_item].lookaheads.add(next_symbol)

    repeat = True
    while repeat:
        repeat = False
        for i_state_id in range(len(table)):
            for i_item, i_cell in table[i_state_id].items():
                # For every kernel item i_item's lookaheads propagate to
                for j_state_id, j_item in i_cell.propagates_to:
                    j_cell = table[j_state_id][j_item]
                    j_cell_lookaheads_len = len(j_cell.lookaheads)
                    j_cell.lookaheads.update(i_cell.lookaheads)
                    if j_cell_lookaheads_len < len(j_cell.lookaheads):
                        repeat = True

    result = [set() for i in range(n_states)]
    for i_state_id in range(n_states):
        for i_item, i_cell in table[i_state_id].items():
            for sym in i_cell.lookaheads:
                item_set = (i_item, sym)
                result[i_state_id].add(item_set)
        result[i_state_id] = closure(gr, result[i_state_id])

    return result


def closure(gr, item_set):
    result = set(item_set)
    current = item_set

    while len(current) > 0:
        new_elements = []
        for ((prod_index, dot), lookahead) in current:
            pname, pbody, plambda = gr.productions[prod_index]
            if dot == len(pbody) or pbody[dot] not in gr.nonterms:
                continue
            nt = pbody[dot]
            nt_offset = gr.nonterm_offset[nt]
            following_symbols = pbody[dot + 1:] + [lookahead]
            following_terminals = gr.first_set(following_symbols) - {None}
            for idx in range(len(nt.productions)):
                for term in following_terminals:
                    new_item_set = ((nt_offset + idx, 0), term)
                    if new_item_set not in result:
                        result.add(new_item_set)
                        new_elements += [new_item_set]
        current = new_elements
    return frozenset(result)


class Error(Exception, abc.ABC):
    @abc.abstractproperty
    def message(self):
        pass


@dataclasses.dataclass
class ParseError(Error):
    pos : Position
    unexpected : Symbol
    expected : list

    @property
    def message(self):
        expected = ', '.join(map(str, self.expected))
        return f'Неожиданный символ {self.unexpected}, ' \
                + f'ожидалось {expected}'


class Parser(object):
    def __init__(self, start_nonterminal):
        fake_axiom = NonTerminal(START_SYMBOL)
        fake_axiom |= start_nonterminal

        self.nonterms = []
        self.terminals = set()
        self.symbols = ()
        self.productions = []
        self.nonterm_offset = {}
        self.__first_sets = {}

        def register(symbol):
            if isinstance(symbol, BaseTerminal):
                self.terminals.add(symbol)
            else:
                assert(isinstance(symbol, NonTerminal))
                if symbol not in self.nonterms:
                    self.nonterms.append(symbol)

            return symbol

        register(fake_axiom)

        scanned_count = 0
        while scanned_count < len(self.nonterms):
            last_unscanned = len(self.nonterms)

            for nt_idx in range(scanned_count, last_unscanned):
                nt = self.nonterms[nt_idx]
                self.nonterm_offset[nt] = len(self.productions)

                for prod, func in nt.enum_rules():
                    for symbol in prod:
                        register(symbol)
                    self.productions.append((nt, prod, func))

            scanned_count = last_unscanned

        self.terminals = tuple(sorted(self.terminals, key=id))
        self.nonterms = tuple(sorted(self.nonterms, key=lambda nt: nt.name))
        self.symbols = self.nonterms + self.terminals
        self.skipped_domains = []

        self.__build_first_sets()
        self.table = ParsingTable(self)

    def first_set(self, x):
        result = set()
        skippable_symbols = 0
        for sym in x:
            fs = self.__first_sets.get(sym, {sym})
            result.update(fs - {None})
            if None in fs:
                skippable_symbols += 1
            else:
                break
        if skippable_symbols == len(x):
            result.add(None)
        return frozenset(result)

    def __build_first_sets(self):
        for s in self.nonterms:
            self.__first_sets[s] = set()
            if [] in s.productions:
                self.__first_sets[s].add(None)

        repeat = True
        while repeat:
            repeat = False

            for nt, prod, func in self.productions:
                curfs = self.__first_sets[nt]
                curfs_len = len(curfs)
                curfs.update(self.first_set(prod))

                if len(curfs) > curfs_len:
                    repeat = True

        self.__first_sets = {x: frozenset(y) for x, y in self.__first_sets.items()}

    def stringify(self, indexes=True):
        lines = '\n'.join(nt.stringify() for nt in self.nonterms)
        if indexes:
            lines = '\n'.join(RULE_INDEXING_PATTERN % (x, y)
                              for x, y in enumerate(lines.split('\n')))
        return lines

    def __str__(self):
        return self.stringify()

    def add_skipped_domain(self, regex):
        self.skipped_domains.append(regex)

    def parse(self, text):
        lexer = Lexer(self.terminals, text, self.skipped_domains)
        stack = [(0, Fragment(Position(), Position()), None)]
        cur = lexer.next_token()
        while True:
            cur_state, cur_coord, top_attr = stack[-1]
            action = next(iter(self.table.action[cur_state][cur.type]), None)
            match action:
                case Shift(state):
                    stack.append((state, cur.pos, cur.attr))
                    cur = lexer.next_token()
                case Reduce(rule):
                    nt, prod, fold = self.productions[rule]
                    n = len(prod)
                    attrs = [attr for state, coord, attr in stack[len(stack)-n:]
                             if attr != None]
                    coords = [coord for state, coord, attr in stack[len(stack)-n:]]
                    if len(coords) > 0:
                        res_coord = Fragment(coords[0].start, coords[-1].following)
                    else:
                        res_coord = Fragment(cur.pos.start, cur.pos.start)
                    del stack[len(stack)-n:]
                    goto_state = self.table.goto[stack[-1][0]][nt]
                    res_attr = fold.callee(attrs, coords, res_coord)
                    stack.append((goto_state, res_coord, res_attr))
                case Accept():
                    assert(len(stack) == 2)
                    return top_attr
                case None:
                    expected = [symbol for symbol, actions
                                in self.table.action[cur_state].items()
                                if len(actions) > 0]
                    raise ParseError(pos=cur.pos.start, unexpected=cur,
                                     expected=expected)

    def tokenize(self, text):
        lexer = Lexer(self.terminals, text, self.skipped_domains)

        while True:
            token = lexer.next_token()
            yield token
            if token.type == EOF_SYMBOL:
                break

    def is_lalr_one(self):
        return self.table.is_lalr_one()

    def print_table(self, file=sys.stdout):
        print(self.table.stringify(), file=file)


def goto(gr, item_set, inp):
    result_set = set()
    for (item, lookahead) in item_set:
        prod_id, dot = item
        pname, pbody, plambda = gr.productions[prod_id]
        if dot == len(pbody) or pbody[dot] != inp:
            continue

        new_item = ((prod_id, dot + 1), lookahead)
        result_set.add(new_item)

    result_set = closure(gr, result_set)
    return result_set


def kernels(item_set):
    return frozenset((item, nextsym) for item, nextsym in item_set if item[1] > 0 or item[0] == 0)


def drop_itemset_lookaheads(itemset):
    return frozenset((x[0], x[1]) for x, y in itemset)


def describe_grammar(gr):
    return '\n'.join([
        'Grammar rules (%d in total):' % len(gr.productions),
        str(gr) + '\n',
        'Grammar non-terminals (%d in total):' % len(gr.nonterms),
        '\n'.join('\t' + str(s) for s in gr.nonterms) + '\n',
        'Grammar terminals (%d in total):' % len(gr.terminals),
        '\n'.join('\t' + str(s) for s in gr.terminals)
    ])


def describe_parsing_table(table):
    conflict_status = table.get_conflict_status()

    def conflict_status_str(state_id):
        has_sr_conflict = (conflict_status[state_id] == STATUS_SR_CONFLICT)
        status_str = ('shift-reduce' if has_sr_conflict else 'reduce-reduce')
        return 'State %d has a %s conflict' % (state_id, status_str)

    return ''.join([
        'PARSING TABLE SUMMARY\n',
        'Is the given grammar LALR(1)? %s\n' % ('Yes' if table.is_lalr_one() else 'No'),
        ''.join(conflict_status_str(sid) + '\n' for sid in range(table.n_states)
                if conflict_status[sid] != STATUS_OK) + '\n',
        table.stringify()
    ])


RULE_INDEXING_PATTERN = '%-5d%s'
START_SYMBOL = '$accept'

STATUS_OK = 0
STATUS_SR_CONFLICT = 1
STATUS_RR_CONFLICT = 2


class LR0_Automaton:
    def __init__(self, gr):
        self.states = []
        self.id_from_state = dict()
        self.goto = dict()

        self.states = [LR0_Automaton.__closure(gr, [(0, 0)])]
        next_id = 0

        self.id_from_state[self.states[-1]] = next_id
        next_id += 1

        seen = set(self.states)
        set_queue = self.states
        while len(set_queue) > 0:
            new_elements = []
            for item_set in set_queue:
                item_set_id = self.id_from_state[item_set]
                for symbol in gr.symbols:
                    next_item_set = LR0_Automaton.__goto(gr, item_set, symbol)
                    if len(next_item_set) == 0:
                        continue
                    if next_item_set not in seen:
                        new_elements += [next_item_set]
                        seen.add(next_item_set)
                        self.states += [next_item_set]
                        self.id_from_state[self.states[-1]] = next_id
                        next_id += 1
                    self.goto[(item_set_id, symbol)] = self.id_from_state[next_item_set]
            set_queue = new_elements

    @staticmethod
    def __closure(gr, item_set):
        result = set(item_set)
        set_queue = item_set
        while len(set_queue) > 0:
            new_elements = []
            for itemProdId, dot in set_queue:
                pname, pbody, plambda = gr.productions[itemProdId]
                if dot == len(pbody) or pbody[dot] not in gr.nonterms:
                    continue
                nt = pbody[dot]
                nt_offset = gr.nonterm_offset[nt]
                for idx in range(len(nt.productions)):
                    new_item_set = (nt_offset + idx, 0)
                    if new_item_set not in result:
                        new_elements += [new_item_set]
                        result.add(new_item_set)
            set_queue = new_elements
        return frozenset(result)


    @staticmethod
    def __goto(gr, item_set, inp):
        result_set = set()
        for prod_index, dot in item_set:
            pname, pbody, plambda = gr.productions[prod_index]
            if dot < len(pbody) and pbody[dot] == inp:
                result_set.add((prod_index, dot + 1))
        result_set = LR0_Automaton.__closure(gr, result_set)
        return result_set


    @staticmethod
    def __kernels(item_set):
        return frozenset((x, y) for x, y in item_set if y > 0 or x == 0)

    def kstates(self):
        return [LR0_Automaton.__kernels(st) for st in self.states]


class LexerError(Error):
    ERROR_SLICE = 10

    def __init__(self, pos, text):
        self.pos = pos
        self.bad = text[pos.offset:pos.offset + self.ERROR_SLICE]

    def __repr__(self):
        return f'LexerError({self.pos!r},{self.bad!r})'

    @property
    def message(self):
        return f'Не удалось разобрать {self.bad!r}'


class Lexer:
    def __init__(self, domains, text, skip):
        self.domains = list(domains)
        self.text = text
        self.pos = Position()
        self.skip_token = object()
        self.domains += [Terminal('-skip-', regex, lambda _: self.skip_token)
                         for regex in skip]
        self.domains.append(ErrorTerminal())

    def next_token(self):
        while self.pos.offset < len(self.text):
            offset = self.pos.offset
            matches = [(d, d.priority, *d.match(self.text, offset))
                       for d in self.domains]
            domain, priority, length, attr = \
                    max(matches, key=lambda t: (t[2], t[1]))

            assert length > 0

            if attr == ErrorTerminal:
                raise LexerError(self.pos, self.text)

            new_pos = self.pos.shift(self.text[offset:offset + length])
            frag = Fragment(self.pos, new_pos)
            self.pos = new_pos
            if attr != self.skip_token:
                token = Token(domain, frag, attr)
                return token

        return Token(EOF_SYMBOL, Fragment(self.pos, self.pos), None)
