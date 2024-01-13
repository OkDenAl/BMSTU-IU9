echo "Test 1 - Matrix size: 8192" > results1.txt
for num in 1 2 4 8 16
do
    mpiexec -n $num --oversubscribe python3 main.py $num >> results1.txt
done