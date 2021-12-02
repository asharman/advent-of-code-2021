DAY="${1}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
INPUT="src/Day${DAY}.elm"
OUTPUT="build/${DAY}.js"
PUZZLE_URL="https://adventofcode.com/2021/day/${DAY_NO_ZEROS}/input"
PUZZLE_FILE="input/2021-${DAY}.txt"

if [ ! -e "${PUZZLE_FILE}" ] ; then
    curl "https://adventofcode.com/2021/day/${DAY_NO_ZEROS}/input" -H "Cookie: session=${AOC_COOKIE_SESSION}" > "${PUZZLE_FILE}"
fi
if [ ! -e "${INPUT}" ] ; then
    # echo "port module Day${DAY} exposing (..)\n${eval }" > ${INPUT}
    printf '%s\n%s\n' "port module Day${DAY} exposing (..)" "$(cat template.elm)" > ${INPUT}
fi
npx elm make "${INPUT}" --optimize --output "${OUTPUT}"
echo "-------------------------------------------------"
node index.js "${DAY}"