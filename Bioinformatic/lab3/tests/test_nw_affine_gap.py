import src.nw_affine_gap as align

def test_nw_affine_gap_1():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert score == 20

def test_nw_affine_gap_2():
    aln1, aln2, score = align.needleman_wunsch_affine("ACG", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACG-"
    assert aln2 == "ACGT"
    assert score == 5

def test_nw_affine_gap_3():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "ACG")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "ACG-"
    assert score == 5

def test_nw_affine_gap_4():
    aln1, aln2, score = align.needleman_wunsch_affine("ACAGT", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACAGT"
    assert aln2 == "AC-GT"
    assert score == 10

def test_nw_affine_gap_5():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "ACAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "AC-GT"
    assert aln2 == "ACAGT"
    assert score == 10

def test_nw_affine_gap_6():
    aln1, aln2, score = align.needleman_wunsch_affine("CAGT", "ACAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "-CAGT"
    assert aln2 == "ACAGT"
    assert score == 10

def test_nw_affine_gap_7():
    aln1, aln2, score = align.needleman_wunsch_affine("ACAGT", "CAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACAGT"
    assert aln2 == "-CAGT"
    assert score == 10

def test_nw_affine_gap_8():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "A")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "A---"
    assert score == -7

def test_nw_affine_gap_9():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "----"
    assert score == -13

def test_nw_affine_gap_10():
    aln1, aln2, score = align.needleman_wunsch_affine("A", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "A---"
    assert aln2 == "ACGT"
    assert score == -7

def test_nw_affine_gap_11():
    aln1, aln2, score = align.needleman_wunsch_affine("", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "----"
    assert aln2 == "ACGT"
    assert score == -13

def test_nw_affine_gap_12():
    aln1, aln2, score = align.needleman_wunsch_affine("", "")
    assert aln1 == ""
    assert aln2 == ""
    assert score == 0

def test_nw_affine_gap_13():
    aln1, aln2, score = align.needleman_wunsch_affine("TACGT", "ATGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "TACGT"
    assert aln2 == "-ATGT"
    assert score == 1

def test_nw_affine_gap_14():
    aln1, aln2, score = align.needleman_wunsch_affine("TACGT", "ACTGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "TAC-GT"
    assert aln2 == "-ACTGT"
    assert score == 0
    
def test_nw_affine_gap_15():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "TAGTA")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT-"
    assert aln2 == "TAGTA"
    assert score == -8

def test_nw_affine_gap_16():
    aln1, aln2, score = align.needleman_wunsch_affine("TAGTA", "ACGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "TAGTA"
    assert aln2 == "ACGT-"
    assert score == -8

def test_nw_affine_gap_17():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "TAGT", gap_open=-1, gap_extend=0)
    assert len(aln1) == len(aln2)
    assert aln1 == "-ACGT"
    assert aln2 == "TA-GT"
    assert score == 13

def test_nw_affine_gap_18():
    aln1, aln2, score = align.needleman_wunsch_affine("TAGT", "ACGT", gap_open=10, gap_extend=10)
    assert len(aln1) == len(aln2)
    assert len(aln1) == 8
    assert score == 80

def test_nw_affine_gap_19():
    aln1, aln2, score = align.needleman_wunsch_affine("GGAGCCAAGGTGAAGTTGTAGCAGTGTGTCC", 
                                   "GACTTGTGGAACCTCTGTCCTCCGAGCTCTC", gap_open=-5, gap_extend=-5)
    assert len(aln1) == len(aln2)
    assert len(aln1) == 36
    assert score == 8

def test_nw_affine_gap_20():
    aln1, aln2, score = align.needleman_wunsch_affine("AAAAAAATTTTTTT", "TTTTTTTAAAAAAA", gap_open=-5, gap_extend=-5)
    assert len(aln1) == len(aln2)
    assert len(aln1) == 21
    assert score == -35

def test_nw_affine_gap_21():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGGCTT", "ACGT")
    assert aln1 == "ACGGCTT"
    assert aln2 == "ACG---T"
    assert score == 8

def test_nw_affine_gap_22():
    aln1, aln2, score = align.needleman_wunsch_affine("ACGT", "TAGT")
    assert len(aln1) == len(aln2)
    assert aln1 == "ACGT"
    assert aln2 == "TAGT"
    assert score == 2