#/bin/sh
RESPATH="$(pwd)"
RESULT="$RESPATH/result.txt"

if [ $# = 2 ]; then
 TEST=$2
 TRANING=$1
else
 echo 'Error: need arguments'
 exit 1
fi

if [ ! -d $TRANING ]; then
 echo "Error: $TRANING not found"
 exit 1
fi

if [ ! -d $TEST ]; then
 echo "Error: $TEST not found"
 exit 1
fi

exec 1>$RESPATH/REPORT.html

echo '<!doctype html>' 	
echo '<html><head><meta charset="utf-8"/><style> img { border: 1px solid #00f; } hr { border:1px solid #000;} table {width:700px;} td:first-child { color:#f00; text-align:right; } td { text-align:center; } </style> </head><body>' 
echo '<h1>Классификатора на основе НС Хемминга:</h1>' 
echo '<h1>Отчет о распознавание образов</h1>' 

echo '<h2>Учебный набор:</h2>'
echo '<table><tr><td></td>' 
i=0;
#traningSets=$(ls -Cf $TRANING/*.bmp)
for f in $TRANING/*.bmp; do
   #echo $f >&2
   echo "<td><img src='$f'/></td>"
   img[$i]=$f
   let "i++"
done
echo '</tr></table><hr/><p/>'

echo '<h2>Тестовый набор:</h2>'
testSetsNumber=$(echo $TEST | tr / "\n" |tail -1)
echo "<table><tr><td>$testSetsNumber</td>"
for f in $TEST/*.bmp; do
   #echo $f >&2
   echo "<td><img src='$f'/></td>" 
done
echo '</tr></table><hr/><p/>'

echo '<h2>Результат работы:</h2>'
resultSet=($(cat $RESULT))
lengthSet=${#resultSet[@]}
k=1
echo "<table><tr><td>Выборка №$k</td>"
for((i=0; i<${lengthSet}-1; i++)); do
  if [ ${resultSet[$i]} == '<--->' ] ; then
    let "k++"
    echo "<table><tr><td>Выборка №$k</td>"
  else
    f=${img[${resultSet[$i]}]}
    echo "<td><img src='$f'/></td>"
  fi
done

echo '</tr></table></body></html>'
