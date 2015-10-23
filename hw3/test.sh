echo "Usage: bash test.sh <number of threads> <number of transitions>"

java="java"

echo "Unsychronized test: "
for i in {1..20}
do
    $java UnsafeMemory Unsynchronized $1 $2 100 5 6 3 0 3 2>&1 | grep -Eow "[0-9.]+" | tr '\n' ' '
    echo ""
done

echo "BetterSorry test: "
for i in {1..20}
do
    $java UnsafeMemory BetterSorry $1 $2 100 5 6 3 0 3 | grep -Eow "[0-9.]+" | tr '\n' ' '
    echo ""
done

echo "GetNSet test: "
for i in {1..20}
do
    $java UnsafeMemory GetNSet $1 $2 100 5 6 3 0 3 | grep -Eow "[0-9.]+" | tr '\n' ' '
    echo ""
done

echo "BetterSafe test: "
for i in {1..20}
do
    $java UnsafeMemory BetterSafe $1 $2 100 5 6 3 0 3 | grep -Eow "[0-9.]+" | tr '\n' ' '
    echo ""
done

echo "Synchronized test: "
for i in {1..20}
do
    $java UnsafeMemory Synchronized $1 $2 100 5 6 3 0 3 | grep -Eow "[0-9.]+" | tr '\n' ' '
    echo ""
done

echo "Null test: "
for i in {1..20}
do
    $java UnsafeMemory Null $1 $2 100 5 6 3 0 3 | grep -Eow "[0-9.]+" | tr '\n' ' '
    echo ""
done


