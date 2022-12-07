# https://adventofcode.com/2022/day/1

# Part 1

cat src/main/resources/y2022/input-day1.txt | \
  gawk '/^[0-9]+$/ { SUM = SUM + $1;} /^$/ { if (SUM > MAX) {MAX=SUM;}; SUM = 0;}; END { print("Max " MAX);}'

# Max 71780


# Part 2

cat src/main/resources/y2022/input-day1.txt | \
  gawk '/^[0-9]+$/ { SUM = SUM + $1;} /^$/ { print(SUM); SUM = 0;};' | \
  sort -nr | \
  head -n 3 | \
  awk '{SUM = SUM + $1}; END {print SUM};'

# 212489
