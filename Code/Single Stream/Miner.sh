#!/bin/sh

printf '%s | %s | %s | %s | %s | %s\n' 'Brute Force' 'Analytics' 'Greedy' 'List' 'Delta' '%'
printf '%.0s-' {1..60}
printf '\n'

numberOfTests=10
total=0

for index in $(seq 1 $numberOfTests); do
    $(./DataMaker)
    bruteForce=$(./BruteForceSerialMethod | tail -2 | head -1 | grep -Eo '[0-9]+$')
    analytics=$(./AnalyticsSerialMethod | tail -2 | head -1 | grep -Eo '[0-9]+$')
    greedy=$(./GreedySerialMethod | tail -2 | head -1 | grep -Eo '[0-9]+$')
    list=$(./ListSerialMethod | tail -2 | head -1 | grep -Eo '[0-9]+$')

    delta=$(($list-$bruteForce))
    percent=$(($delta * 100 / $bruteForce))
    total=$(($total + $percent))

    printf '%-11s   %-9s   %-6s   %-4s   %-5s   %s\n' $bruteForce $analytics $greedy $list $delta $percent
    if [ $delta \< 0 ]; then
        echo 'Abort'
        exit
    fi 
done

printf '\nAverage delta: %s%%\n' $(($total / $numberOfTests))