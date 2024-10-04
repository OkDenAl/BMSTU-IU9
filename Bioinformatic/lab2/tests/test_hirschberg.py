import hirschberg.align as align


def test_hirschberg_1():
    aln1, aln2, score = align.hirschberg("ACGT", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert score == 20


def test_hirschberg_2():
    aln1, aln2, score = align.hirschberg("ACG", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACG-"
    assert aln2 == "ACGT"
    assert score == 10


def test_hirschberg_3():
    aln1, aln2, score = align.hirschberg("ACGT", "ACG")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "ACG-"
    assert score == 10


def test_hirschberg_4():
    aln1, aln2, score = align.hirschberg("ACAGT", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACAGT"
    assert aln2 == "AC-GT"
    assert score == 15


def test_hirschberg_5():
    aln1, aln2, score = align.hirschberg("ACGT", "ACAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "AC-GT"
    assert aln2 == "ACAGT"
    assert score == 15


def test_hirschberg_6():
    aln1, aln2, score = align.hirschberg("CAGT", "ACAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "-CAGT"
    assert aln2 == "ACAGT"
    assert score == 15


def test_hirschberg_7():
    aln1, aln2, score = align.hirschberg("ACAGT", "CAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACAGT"
    assert aln2 == "-CAGT"
    assert score == 15


def test_hirschberg_8():
    aln1, aln2, score = align.hirschberg("ACGT", "A")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "A---"
    assert score == -10


def test_hirschberg_9():
    aln1, aln2, score = align.hirschberg("ACGT", "")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "----"
    assert score == -20


def test_hirschberg_10():
    aln1, aln2, score = align.hirschberg("A", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "A---"
    assert aln2 == "ACGT"
    assert score == -10


def test_hirschberg_11():
    aln1, aln2, score = align.hirschberg("", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "----"
    assert aln2 == "ACGT"
    assert score == -20


def test_hirschberg_12():
    aln1, aln2, score = align.hirschberg("", "")
    assert aln1 == ""
    assert aln2 == ""
    assert score == 0


def test_hirschberg_13():
    aln1, aln2, score = align.hirschberg("TACGT", "ATGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "TACGT"
    assert aln2 == "-ATGT"
    assert score == 6


def test_hirschberg_14():
    aln1, aln2, score = align.hirschberg("TACGT", "ACTGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "TAC-GT"
    assert aln2 == "-ACTGT"
    assert score == 10


def test_hirschberg_15():
    aln1, aln2, score = align.hirschberg("ACGT", "TAGTA")
    assert len(aln1) == len(aln2)
    assert aln1 == "-ACGT-"
    assert aln2 == "TA-GTA"
    assert score == 0


def test_hirschberg_16():
    aln1, aln2, score = align.hirschberg("TAGTA", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "TA-GTA"
    assert aln2 == "-ACGT-"
    assert score == 0


def test_hirschberg_17():
    aln1, aln2, score = align.hirschberg("ACGT", "TAGT", gap_score=0)
    assert len(aln1) == len(aln2)
    assert aln1 == "-ACGT"
    assert aln2 == "TA-GT"
    assert score == 15


def test_hirschberg_18():
    aln1, aln2, score = align.hirschberg("TAGT", "ACGT", gap_score=10)
    assert len(aln1) == len(aln2)
    assert len(aln1) == 8
    assert score == 80


def test_hirschberg_19():
    aln1, aln2, score = align.hirschberg("GGAGCCAAGGTGAAGTTGTAGCAGTGTGTCC",
                                         "GACTTGTGGAACCTCTGTCCTCCGAGCTCTC", gap_score=-5)
    assert len(aln1) == len(aln2)
    assert len(aln1) == 36
    assert score == 8


def test_hirschberg_20():
    aln1, aln2, score = align.hirschberg("AAAAAAATTTTTTT", "TTTTTTTAAAAAAA", gap_score=-5)
    assert len(aln1) == len(aln2)
    assert len(aln1) == 21
    assert score == -35
