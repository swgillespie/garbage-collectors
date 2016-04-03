set -e

while [[ $# > 1 ]]
do
    key="$1"

    case $key in
        -r|--runtime)
            RUNTIME="$2"
            shift # past argument
            ;;
        -s|--source-file)
            SOURCE="$2"
            shift # past argument
            ;;
        -h|--help)
            DISPLAY_HELP="YES"
            ;;
        *)
            # unknown option
            ;;
    esac
    shift # past argument or value
done

#if [ -z "$DISPLAY_HELP" ]; then
#    echo "usage: compile.sh [options]"
#    echo "   -r|--runtime     - The runtime folder to link the target executable against"
#    echo "   -s|--source-file - The source file to compile"
#    echo "   -h|--help        - Show this message"
#    exit 0
#fi

if [ -z "$RUNTIME" ]; then
    echo "please specify a runtime to use"
    exit 1
fi

if [ -z "$SOURCE" ]; then
    echo "please specify a source file to compile"
    exit 1
fi

echo "Remaking compiler..."
make -C compiler/ > /dev/null 2>&1
echo "Remaking chosen runtime $RUNTIME..."
make -C $RUNTIME > /dev/null 2>&1
echo "Compiling $SOURCE..."
./compiler/shortyc $SOURCE
SOURCE_LL="${SOURCE%.*}.ll"
SOURCE_O="${SOURCE%.*}.o"
SOURCE_EXE="${SOURCE%.*}"
echo "Optimizing $SOURCE..."
opt -O2 -rewrite-statepoints-for-gc -S $SOURCE_LL | llc -O2 -filetype=obj -o $SOURCE_O -
echo "Linking $SOURCE..."
gcc -static -o $SOURCE_EXE $SOURCE_O $RUNTIME/libshorty_runtime.a
rm $SOURCE_LL
rm $SOURCE_O
echo "Done!"
