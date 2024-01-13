#include <ctime>
#include <stdio.h>
#include <iostream>
#include <math.h>
#include <omp.h>

using namespace std;

const long long N = 512;
const double eps = 0.0000001;
const double t = 0.1 / N;
const int NUM_THREADS = 8;

void fillMatrix(double* matrix) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            if (i == j) matrix[i * N + j] = 2.0;
            else matrix[i * N + j] = 1.0;
        }
    }
}

void fillB(double* B) {
    for (int i = 0; i < N; i++) {
        B[i] = N + 1;
    }
}

void fillX(double* x) {
    for (int i = 0; i < N; i++) {
        x[i] = 0;
    }
}

void printMatrix(double* matrix) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            printf("%f ", matrix[i * N + j]);
        }
        printf("\n");
    }
}

void printVector(double* v) {
    cout << v[0];
    for (int i = 1; i < N; i++) {
        cout << ", " << v[i];
    }
    cout << endl;
}

void printX(double* x, int size) {
    cout << "Answer: " << x[0];
    for (int i = 1; i < size; i++) {
        cout << ", " << x[i];
    }
    cout << endl;
}

bool isAnswerCorrect(double* x) {
    for (int i = 0; i < N; i++) {
        if (abs(x[i] - 1.0) < 0.00001) {
            continue;
        }
        else {
            cout << "here " << i << " " << x[i] << endl;
            return false;
        }
    }
    return true;
}

double getNorm(double* v) {
    double s = 0;
    int i;
    omp_set_num_threads(NUM_THREADS);
    #pragma omp parallel for shared(v) private(i) reduction(+:s)
    for (int i = 0; i < N; i++) {
        s += pow(v[i], 2);
    }
    cout<< "I am thread " << omp_get_num_threads() << endl;
    s = sqrt(s);
    return s;
}

double* mulMatrixByVector(double* matrix, double* x) {
    
    double* res = new double[N];
    int i = 0;
    omp_set_num_threads(NUM_THREADS);
    #pragma omp parallel for shared(matrix, x, res) private(i)
    for (i = 0; i < N; i++) {
        res[i] = 0;
        for (int j = 0; j < N; j++) {
            res[i] += matrix[i * N + j] * x[j];
        }
    }
    cout << "I am thread " << omp_get_num_threads() << endl;

    return res;
}

void procFunc( double* A, double* B, double* x, double* y) {
    double condition = eps;
    int k = 0;
    while (true) {
        cout << "_________________________iteration = " << k << "____________________________" << endl;
        k++;
        y = mulMatrixByVector(A, x);
        int i;
        omp_set_num_threads(NUM_THREADS);
        #pragma omp parallel shared(y, B) private(i) 
        {
            cout << "I am thread " << omp_get_num_threads() << endl;
            #pragma omp for
            for (i = 0; i < N; i++) {
                y[i] -= B[i];
            }
        }
        
        condition = getNorm(y) / getNorm(B);
        
        if (condition < eps) {
            break;
        }
        omp_set_num_threads(NUM_THREADS);
        #pragma omp parallel shared(y) private(i) 
        {
            cout << "I am thread " << omp_get_num_threads() << endl;
            #pragma omp for
            for (i = 0; i < N; i++) {
                y[i] *= t;
            }
        }
        omp_set_num_threads(NUM_THREADS);
        #pragma omp parallel shared(x, y) private(i) 
        {
            cout << "I am thread " << omp_get_num_threads() << endl;
            #pragma omp for
            for (i = 0; i < N; i++) {
                x[i] -= y[i];
            }
        }
        
        printf("condition is now: %lf\n", condition);
    }
}

int main(int argc, char* argv[]) {
    #ifdef _OPENMP
        printf("OpenMP is supported!\n");
    #endif // _OPENMP

    double* A = new double[N * N];
    double* B = new double[N];
    double* x = new double[N];
    double* y = new double[N];

    fillX(x);
    
    printf("Generating matrix A, size %lldx%lld\n", N, N);
    fillMatrix(A);
    //printMatrix(A);
    printf("Generating vector B, size %lld\n", N);
    fillB(B);
    //printVector(B);
    fillX(y);
    printf("\n----------------Starting Calculations----------------\n");
    
    double start_time = omp_get_wtime();
    procFunc(A, B, x, y);
    double end_time = omp_get_wtime();

    printf("\n----------------Ending Calculations----------------\n");
    cout << "duration is " << end_time - start_time << " seconds for " << NUM_THREADS << " threads\nfor matrix "
        << N << "x" << N << endl;
    printX(x, N);
    if (!isAnswerCorrect(x)) {
        printf("\nERROR - THE WRONG ANSWER\n");
    }
    else {
        printf("\n!!THE RIGHT ANSWER!!\n");
    }

    delete[] A;
    delete[] B;
    delete[] x;
    delete[] y;

    return 0;
}