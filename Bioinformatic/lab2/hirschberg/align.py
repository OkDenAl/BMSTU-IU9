from copy import copy
from typing import Callable, Tuple

DEBUG = False


def score_fun(a: str,
              b: str,
              match_score: int = 5,
              mismatch_score: int = -4) -> int:
    return match_score if a == b else mismatch_score


def hirschberg(seq1: str,
               seq2: str,
               score_fun: Callable = score_fun,
               gap_score: int = -5) -> Tuple[str, str, int]:
    '''
    Inputs:
    seq1 - first sequence
    seq2 - second sequence
    score_fun - function that returns score for two symbols
    gap_score - score for gap in final alignment

    Outputs:
    aln1 - first sequence in alignment
    aln2 - second sequence in alignment
    score - score of alignment
    '''
    seq1_len = len(seq1)
    seq2_len = len(seq2)

    # если одна из последовательностей имеет длину меньше 2,
    # имеет смысл воспользоваться алгоритмом Нидльмана-Вунша
    if seq1_len < 2 or seq2_len < 2:
        return needleman_wunsch(seq1, seq2, score_fun, gap_score)

    mid_ind = seq1_len // 2
    left = needleman_wunsch_last_row(seq1[:mid_ind], seq2, score_fun, gap_score)
    right = needleman_wunsch_last_row(seq1[mid_ind:][::-1], seq2[::-1], score_fun, gap_score)[::-1]

    s = [l + r for l, r in zip(left, right)]
    # индекс максимального элемента
    max_ind = s.index(max(s))

    # рекурсивно вызываем алгоритм
    l_al_seq1, l_al_seq2, l_al_score = hirschberg(seq1[:mid_ind], seq2[:max_ind], gap_score=gap_score)
    r_al_seq1, r_al_seq2, r_al_score = hirschberg(seq1[mid_ind:], seq2[max_ind:], gap_score=gap_score)

    # суммируем полученные результаты
    return l_al_seq1 + r_al_seq1, l_al_seq2 + r_al_seq2, l_al_score + r_al_score


def needleman_wunsch_last_row(seq1: str, seq2: str, score_fun: Callable = score_fun, gap_score: int = -5):
    seq1_len = len(seq1) + 1
    seq2_len = len(seq2) + 1

    # первая строка матрицы
    prev_row = [0] * seq2_len
    for i in range(seq2_len):
        prev_row[i] = i * gap_score

    # текущая строка матрицы (последняя на i-й итерации)
    cur_row = copy(prev_row)

    for i in range(1, seq1_len):
        # заполняем значение 1ого столбца в алгоритме Нидльмана-Вунша
        cur_row[0] = i * gap_score
        for j in range(1, seq2_len):
            # считаем скоры аналогично алгоритму Нидльмана-Вунша
            match_score = prev_row[j - 1] + score_fun(seq1[i - 1], seq2[j - 1])
            delete_score = prev_row[j] + gap_score
            insert_score = cur_row[j - 1] + gap_score
            cur_row[j] = max(match_score, delete_score, insert_score)
        # предыдущая строка становится текущей
        # (т.е в следующей итерации смотрим следующую строку, опираясь на прошлую)
        prev_row = cur_row[:]

    # возвращаем последнюю строку матрицы
    return cur_row


def needleman_wunsch(seq1: str, seq2: str, score_fun: Callable = score_fun, gap_score: int = -5):
    m, n = len(seq1) + 1, len(seq2) + 1

    matrix = [[0] * n for _ in range(m)]

    for i in range(m):
        matrix[i][0] = i * gap_score
    for j in range(n):
        matrix[0][j] = j * gap_score

    for i in range(1, m):
        for j in range(1, n):
            matrix[i][j] = max(matrix[i - 1][j - 1] + score_fun(seq1[i - 1], seq2[j - 1]),
                               matrix[i - 1][j] + gap_score,
                               matrix[i][j - 1] + gap_score)
    if DEBUG:
        print_array(matrix)

    score = matrix[-1][-1]
    i, j = m - 1, n - 1
    aln1 = ""
    aln2 = ""
    while i > 0 or j > 0:
        a, b = '-', '-'
        # (A, B)
        if i > 0 and j > 0 and matrix[i][j] == matrix[i - 1][j - 1] + score_fun(seq1[i - 1], seq2[j - 1]):
            a = seq1[i - 1]
            b = seq2[j - 1]
            i -= 1
            j -= 1

        # (A, -)
        elif i > 0 and matrix[i][j] == matrix[i - 1][j] + gap_score:
            a = seq1[i - 1]
            i -= 1

        # (-, A)
        elif j > 0 and matrix[i][j] == matrix[i][j - 1] + gap_score:
            b = seq2[j - 1]
            j -= 1

        aln1 += a
        aln2 += b
    return aln1[::-1], aln2[::-1], score


def print_array(matrix: list):
    for row in matrix:
        for element in row:
            print(f"{element:6}", end="")
        print()


if __name__ == "__main__":
    aln1, aln2, score = hirschberg("ATCT", "ACT", gap_score=-5)
    assert len(aln1) == len(aln2)
    print(aln1)
    print(aln2)
    print(score)

    aln1, aln2, score = needleman_wunsch("ATCT", "ACT", gap_score=-5)

    assert len(aln1) == len(aln2)
    print(aln1)
    print(aln2)
    print(score)
