from typing import Callable, Tuple

DEBUG = False


def score_fun(a: str,
              b: str,
              match_score: int = 5,
              mismatch_score: int = -4) -> int:
    return match_score if a == b else mismatch_score


def needleman_wunsch_affine(seq1: str,
                            seq2: str,
                            score_fun: Callable = score_fun,
                            gap_open: int = -10,
                            gap_extend: int = -1) -> Tuple[str, str, int]:
    '''
    Inputs:
    seq1 - first sequence
    seq2 - second sequence
    score_fun - function that takes two characters and returns score
    gap_open - gap open penalty
    gap_extend - gap extend penalty
    Outputs:
    aln1 - first aligned sequence
    aln2 - second aligned sequence
    score - score of the alignment
    '''

    # infinity = 2 * gap_open + (n + m - 2) * gap_extend + 1
    infinity = float('-inf')

    # 1. Initialize matrices
    seq1_len = len(seq1) + 1
    seq2_len = len(seq2) + 1
    m_m = [[0 for _ in range(seq2_len)] for _ in range(seq1_len)]
    m_i = [[0 for _ in range(seq2_len)] for _ in range(seq1_len)]
    m_d = [[0 for _ in range(seq2_len)] for _ in range(seq1_len)]

    res = [[0 for _ in range(seq2_len)] for _ in range(seq1_len)]

    m_i[0][0] = infinity
    m_d[0][0] = infinity

    for i in range(1, seq1_len):
        m_m[i][0] = infinity
        m_i[i][0] = gap_open + (i - 1) * gap_extend
        m_d[i][0] = infinity

        res[i][0] = gap_open + (i - 1) * gap_extend  # максимальное значение

    for j in range(1, seq2_len):
        m_m[0][j] = infinity
        m_i[0][j] = infinity
        m_d[0][j] = gap_open + (j - 1) * gap_extend

        res[0][j] = gap_open + (j - 1) * gap_extend  # максимальное значение

    # 2. Fill matrices
    # We assume that consecutive gaps on different sequences are not allowed
    for i in range(1, seq1_len):
        for j in range(1, seq2_len):
            m_m[i][j] = max(
                m_m[i - 1][j - 1],
                m_i[i - 1][j - 1],
                m_d[i - 1][j - 1],
            ) + score_fun(seq1[i - 1], seq2[j - 1])

            m_i[i][j] = max(
                m_i[i][j - 1] + gap_extend,
                m_m[i][j - 1] + gap_open,
            )

            m_d[i][j] = max(
                m_d[i - 1][j] + gap_extend,
                m_m[i - 1][j] + gap_open,
            )

            res[i][j] = max(m_m[i][j], m_i[i][j], m_d[i][j])

    # 3. Traceback
    aln1 = ''
    aln2 = ''
    i = len(seq1)
    j = len(seq2)
    while i > 0 or j > 0:
        a, b = '-', '-'
        if i == 0 or (j > 0 and (res[i][j] == m_m[i][j - 1] + gap_open
                                 or res[i][j] == m_i[i][j - 1] + gap_extend)):
            b = seq2[j - 1]
            j -= 1
        elif j == 0 or (i > 0 and (res[i][j] == m_m[i - 1][j] + gap_open
                                   or res[i][j] == m_d[i - 1][j] + gap_extend)):
            a = seq1[i - 1]
            i -= 1
        elif i > 0 and j > 0 and (res[i][j] == m_m[i - 1][j - 1] + score_fun(seq1[i - 1], seq2[j - 1])
                                  or res[i][j] == m_d[i - 1][j - 1] + score_fun(seq1[i - 1], seq2[j - 1])
                                  or res[i][j] == m_i[i - 1][j - 1] + score_fun(seq1[i - 1], seq2[j - 1])):
            a = seq1[i - 1]
            b = seq2[j - 1]
            i -= 1
            j -= 1

        aln1 += a
        aln2 += b

    score = res[-1][-1]

    return aln1[::-1], aln2[::-1], score


def print_array(matrix: list):
    for row in matrix:
        for element in row:
            print(f"{element:6}", end="")
        print()


def main():
    aln1, aln2, score = needleman_wunsch_affine("ACGT", "TAGT", gap_open=-10, gap_extend=-1)
    print(f'str 1: {aln1}')
    print(f'str 2: {aln2}')
    print(f'score: {score}')


if __name__ == "__main__":
    main()
