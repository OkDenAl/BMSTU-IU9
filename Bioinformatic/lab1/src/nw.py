import argparse
import sys

PRINT_MAX_LINE_LENGTH = 80
DEBUG = False

def score_fun(a: str, 
              b: str,
              match_score: int = 5, 
              mismatch_score: int = -4) -> int:
    return match_score if a == b else mismatch_score


def needleman_wunsch(seq1, seq2, score_fn, gap):
    matrix = [[0 for _ in range(len(seq2) + 1)] for _ in range(len(seq1) + 1)]

    # Заполняем матрицу с помощью соотношений
    for i in range(0, len(seq1) + 1):
        for j in range(0, len(seq2) + 1):
            if j == 0:
                matrix[i][0] = gap * i
            elif i == 0:
                matrix[0][j] = gap * j
            else:
                match_score = matrix[i - 1][j - 1] + score_fn(seq1[i - 1], seq2[j - 1])
                delete_score = matrix[i - 1][j] + gap
                insert_score = matrix[i][j - 1] + gap
                matrix[i][j] = max(match_score, delete_score, insert_score)

    res_seq1 = ""
    res_seq2 = ""
    i = len(seq1)
    j = len(seq2)

    # Восстанавливаем, идя обратно, выбирая шаги из 3х возможных вариантов
    while i > 0 or j > 0:
        score = matrix[i][j]
        match_score = matrix[i - 1][j - 1]
        insert_score = matrix[i][j - 1]
        delete_score = matrix[i - 1][j]

        if i > 0 and j > 0 and score == match_score + score_fn(seq1[i - 1], seq2[j - 1]):
            res_seq1 += seq1[i - 1]
            res_seq2 += seq2[j - 1]
            i -= 1
            j -= 1
        elif j > 0 and score == insert_score + gap:
            res_seq1 += '-'
            res_seq2 += seq2[j - 1]
            j -= 1
        elif i > 0 and score == delete_score + gap:
            res_seq1 += seq1[i - 1]
            res_seq2 += '-'
            i -= 1

    # Если подошли к какой-то границе, идём по границе
    while j > 0:
        res_seq1 += '-'
        res_seq2 += seq2[j - 1]
        j -= 1

    while i > 0:
        res_seq1 += seq1[i - 1]
        res_seq2 += '-'
        i -= 1

    return matrix[-1][-1], res_seq1[::-1], res_seq2[::-1]


def print_array(matrix: list):
    for row in matrix:
        for element in row:
            print(f"{element:6}", end="")
        print()

def print_results(seq1: str, seq2: str, score: int, file = None):
    """Prints the results of the Needleman-Wunsch algorithm.

    This function takes two aligned sequences and the optimal alignment score
    as arguments. It prints the sequences and the score to the standard output
    or to a file.

    Args:
        seq1: The first aligned sequence, e.g. 'ACCGT'
        seq2: The second aligned sequence, e.g. 'AC-GT'
        score: The optimal alignment score, e.g. 10
        file: The file to print to. If None, prints to the standard output.

    Returns:
        None
    """
    if file is None:
        file = sys.stdout

    def print_subseq(i, n, s):
        print("%s: %s" % (n, s[i: i + PRINT_MAX_LINE_LENGTH]), file=file)

    print("Pairwise alignment:", file=file)
    for i in range(0, len(seq1), PRINT_MAX_LINE_LENGTH):
        print_subseq(i, 'seq1', seq1)
        print_subseq(i, 'seq2', seq2)
        print(file=file)
    print("Score: %s" % score, file=file)

def main():
    parser = argparse.ArgumentParser(description='Needleman-Wunsch algorithm')
    parser.add_argument('seq1', help='first sequence')
    parser.add_argument('seq2', help='second sequence')
    parser.add_argument('--match', type=int, help='match score')
    parser.add_argument('--mismatch', type=int, help='mismatch score')
    parser.add_argument('--gap', type=int, default=-10, help='gap penalty')
    parser.add_argument('--debug', action='store_true', help='debug mode')
    args = parser.parse_args()

    global DEBUG
    DEBUG = args.debug
    print(args.match, args.mismatch, args.gap)
    
    if args.match and args.mismatch:
        score, aln1, aln2 = needleman_wunsch(args.seq1, 
                                             args.seq2, 
                                             lambda x, y: args.match if x == y else args.mismatch,
                                             args.gap)
    else:
        assert not args.match and not args.mismatch, "match and mismatch must be specified together"
        score, aln1, aln2 = needleman_wunsch(args.seq1, 
                                             args.seq2, 
                                             score_fun,
                                             args.gap)
    print_results(aln1, aln2, score)

    return score, aln1, aln2

if __name__ == '__main__':
    main()