import src.nw as align


def test_nw_1():
    """Identical sequences, match=5, mismatch=-4, gap=-10
    """
    seq1 = 'ACGT'
    seq2 = 'ACGT'

    # Скор 5*4 = 20 (4 буквы совпали)
    # Последовательности без изменений

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: 5 if x == y else -4,
                                                               gap=-10)
    assert score == 20
    assert aligned_seq1 == 'ACGT'
    assert aligned_seq2 == 'ACGT'


def test_nw_2():
    """Fully different sequences with same length, match=4, mismatch=-3, gap=3
    """
    seq1 = 'ABCDEF'
    seq2 = 'HJKLMN'

    # Скор 12*3 = 36 (6*2 вставок, т.к нет ни одного совпадения букв)
    # ABCDEF------
    # ------HJKLMN

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: 4 if x == y else -3,
                                                               gap=3)

    assert score == 36
    assert aligned_seq1 == 'ABCDEF------'
    assert aligned_seq2 == '------HJKLMN'


def test_nw_3():
    """Reversed sequences with same length, match=5, mismatch=-4, gap=2

    """
    seq1 = 'ABCDFF'
    seq2 = 'FFDCBA'

    # Скор 8*2 + 5*2 = 26 (4*2 вставок + 2 совпадения, т.к цена за совпадение заметно выше, чем за вставку)
    # ABCDFF----
    # ----FFDCBA

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: 5 if x == y else -4,
                                                               gap=2)

    assert score == 26
    assert aligned_seq1 == 'ABCDFF----'
    assert aligned_seq2 == '----FFDCBA'


def test_nw_4():
    """Empty sequences, match=5, mismatch=-4, gap=2

    """
    seq1 = ''
    seq2 = ''

    # Скор 0 при любых значения параметров

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: 5 if x == y else -4,
                                                               gap=2)

    assert score == 0
    assert aligned_seq1 == ''
    assert aligned_seq2 == ''


def test_nw_5():
    """sequences with different length, match=5, mismatch=-4, gap=-10
    """
    seq1 = 'ACBGHT'
    seq2 = 'ACGT'

    # Скор 5*4 - 10*2 = 0 (4 буквы совпали и 2 вставки, т.к цена за вставку очень маленькая, то она только в вынужденных
    # местах, совпадение преобладает)
    # ACBGHT
    # AC-G-T

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: 5 if x == y else -4,
                                                               gap=-10)

    assert score == 0
    assert aligned_seq1 == 'ACBGHT'
    assert aligned_seq2 == 'AC-G-T'


def test_nw_6():
    """sequences with different length, 1 empty, match=5, mismatch=-4, gap=-10
    """
    seq1 = ''
    seq2 = 'ACGT'

    # Скор 10*4 = -40 (4 вставки в любом случае)
    # ----
    # ACGT

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: 5 if x == y else -4,
                                                               gap=-10)

    assert score == -40
    assert aligned_seq1 == '----'
    assert aligned_seq2 == 'ACGT'

def test_nw_7():
    """sequences with different length but mismatch>match>gap, 1 empty, match=-5, mismatch=4, gap=-10
    """
    seq1 = 'ACGTG'
    seq2 = 'ACGT'

    # Скор 4*4 - 10 = 6 (1 вставка, чтобы сместить последовательность и 4 несовпадения)
    # ACGTG
    # -ACGT

    score, aligned_seq1, aligned_seq2 = align.needleman_wunsch(seq1,
                                                               seq2,
                                                               score_fn=lambda x, y: -5 if x == y else 4,
                                                               gap=-10)

    assert score == 6
    assert aligned_seq1 == 'ACGTG'
    assert aligned_seq2 == '-ACGT'
