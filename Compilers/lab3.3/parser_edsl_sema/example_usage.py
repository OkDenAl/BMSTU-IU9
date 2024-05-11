import parser_edsl as pe
import re
import math


CONSTANTS = {
    'pi': math.pi,
    'e': math.e,
    'Na': 6.02214076e23, # Постоянная Больцмана
    'kB': 1.380649e-23,  # Число Авогадро
}


# Нетерминальные символы
Expr = pe.NonTerminal('Expr')
Term = pe.NonTerminal('Term')
Factor = pe.NonTerminal('Factor')


# Терминальные символы
INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)
REAL = pe.Terminal('REAL', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
CONST = pe.Terminal('CONST', '[A-Za-z]+', str)
KW_MOD = pe.Terminal('mod', 'mod', lambda name: None,
                     re_flags=re.IGNORECASE, priority=10)


class SemanticError(pe.Error):
    def __init__(self, pos, message):
        self.pos = pos
        self.__message = message

    @property
    def message(self):
        return self.__message


def checked_div(attrs, coords, res_coord):
    x, y = attrs
    cx, cslash, cy = coords
    if y != 0:
        return x / y
    else:
        raise SemanticError(cslash.start, f'Деление на ноль {x} / {y}')


@pe.ExAction
def checked_mod(attrs, coords, res_coord):
    x, y = attrs
    cx, cmod, cy = coords
    if y != 0:
        return x % y
    else:
        raise SemanticError(cmod.start, f'Остаток при делении на нуль {x} MOD {y}')


@pe.ExAction
def checked_const(attrs, coords, res_coord):
    name, = attrs
    cname, = coords
    if name in CONSTANTS:
        return CONSTANTS[name]
    else:
        raise SemanticError(cname.start, f'Неизвестная константа {name}')


# Правила грамматики
Expr |= Expr, '+', Term, lambda x, y: x + y
Expr |= Expr, '-', Term, lambda x, y: x - y
Expr |= Term

Term |= Term, '*', Factor, lambda x, y: x * y
Term |= Term, '/', Factor, pe.ExAction(checked_div)
Term |= Term, KW_MOD, Factor, checked_mod
Term |= Factor

Factor |= INTEGER
Factor |= '(', Expr, ')'
Factor |= REAL
Factor |= CONST, checked_const


# Парсер
parser = pe.Parser(Expr)
assert parser.is_lalr_one()

parser.add_skipped_domain('\\s')        # пробельные символы
parser.add_skipped_domain('\\{.*?\\}')  # комментарии вида {...}


def test(program):
    print(program)

    try:
        print(parser.parse(program))
    except pe.Error as e:
        print(f'Ошибка {e.pos}: {e.message}')

    print()


test('21 * (1 + { комментарий } 1)')
test('Na * kB')
test('100 mod 7 + 100 MOD 13')
test('(2 + 2) / (2 - 2)')
test('377 mod 0')
test('pi * e * Na * kB')
test('pi * E * Na * kB')
