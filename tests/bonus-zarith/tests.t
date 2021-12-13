Test de fact.p

  $ echo 15 | ../../polish.exe -eval fact.p
  n ? 1
  1
  2
  1
  3
  2
  4
  6
  5
  24
  6
  120
  7
  720
  8
  5040
  9
  40320
  10
  362880
  11
  3628800
  12
  39916800
  13
  479001600
  14
  6227020800
  15
  87178291200
  1307674368000

  $ echo 20 | ../../polish.exe -eval fact.p
  n ? 1
  1
  2
  1
  3
  2
  4
  6
  5
  24
  6
  120
  7
  720
  8
  5040
  9
  40320
  10
  362880
  11
  3628800
  12
  39916800
  13
  479001600
  14
  6227020800
  15
  87178291200
  16
  1307674368000
  17
  20922789888000
  18
  355687428096000
  19
  6402373705728000
  20
  121645100408832000
  2432902008176640000

  $ echo 25 | ../../polish.exe -eval fact.p
  n ? 1
  1
  2
  1
  3
  2
  4
  6
  5
  24
  6
  120
  7
  720
  8
  5040
  9
  40320
  10
  362880
  11
  3628800
  12
  39916800
  13
  479001600
  14
  6227020800
  15
  87178291200
  16
  1307674368000
  17
  20922789888000
  18
  355687428096000
  19
  6402373705728000
  20
  121645100408832000
  21
  2432902008176640000
  22
  51090942171709440000
  23
  1124000727777607680000
  24
  25852016738884976640000
  25
  620448401733239439360000
  15511210043330985984000000

  $ echo 30 | ../../polish.exe -eval fact.p
  n ? 1
  1
  2
  1
  3
  2
  4
  6
  5
  24
  6
  120
  7
  720
  8
  5040
  9
  40320
  10
  362880
  11
  3628800
  12
  39916800
  13
  479001600
  14
  6227020800
  15
  87178291200
  16
  1307674368000
  17
  20922789888000
  18
  355687428096000
  19
  6402373705728000
  20
  121645100408832000
  21
  2432902008176640000
  22
  51090942171709440000
  23
  1124000727777607680000
  24
  25852016738884976640000
  25
  620448401733239439360000
  26
  15511210043330985984000000
  27
  403291461126605635584000000
  28
  10888869450418352160768000000
  29
  304888344611713860501504000000
  30
  8841761993739701954543616000000
  265252859812191058636308480000000

Test de fibo.p

  $ echo 10 | ../../polish.exe -eval fibo.p
  n ? 55
  $ echo 20 | ../../polish.exe -eval fibo.p
  n ? 6765
  $ echo 30 | ../../polish.exe -eval fibo.p
  n ? 832040
  $ echo 40 | ../../polish.exe -eval fibo.p
  n ? 102334155
  $ echo 50 | ../../polish.exe -eval fibo.p
  n ? 12586269025
  $ echo 60 | ../../polish.exe -eval fibo.p
  n ? 1548008755920
  $ echo 70 | ../../polish.exe -eval fibo.p
  n ? 190392490709135
  $ echo 80 | ../../polish.exe -eval fibo.p
  n ? 23416728348467685
  $ echo 90 | ../../polish.exe -eval fibo.p
  n ? 2880067194370816120
  $ echo 100 | ../../polish.exe -eval fibo.p
  n ? 354224848179261915075
  $ echo 110 | ../../polish.exe -eval fibo.p
  n ? 43566776258854844738105
  $ echo 120 | ../../polish.exe -eval fibo.p
  n ? 5358359254990966640871840
  $ echo 130 | ../../polish.exe -eval fibo.p
  n ? 659034621587630041982498215
  $ echo 140 | ../../polish.exe -eval fibo.p
  n ? 81055900096023504197206408605
  $ echo 150 | ../../polish.exe -eval fibo.p
  n ? 9969216677189303386214405760200
  $ echo 160 | ../../polish.exe -eval fibo.p
  n ? 1226132595394188293000174702095995
  $ echo 170 | ../../polish.exe -eval fibo.p
  n ? 150804340016807970735635273952047185
  $ echo 180 | ../../polish.exe -eval fibo.p
  n ? 18547707689471986212190138521399707760
  $ echo 190 | ../../polish.exe -eval fibo.p
  n ? 2281217241465037496128651402858212007295
  $ echo 200 | ../../polish.exe -eval fibo.p
  n ? 280571172992510140037611932413038677189525
  $ echo 210 | ../../polish.exe -eval fibo.p
  n ? 34507973060837282187130139035400899082304280
  $ echo 220 | ../../polish.exe -eval fibo.p
  n ? 4244200115309993198876969489421897548446236915
  $ echo 230 | ../../polish.exe -eval fibo.p
  n ? 522002106210068326179680117059857997559804836265
  $ echo 240 | ../../polish.exe -eval fibo.p
  n ? 64202014863723094126901777428873111802307548623680
  $ echo 250 | ../../polish.exe -eval fibo.p
  n ? 7896325826131730509282738943634332893686268675876375
  $ echo 260 | ../../polish.exe -eval fibo.p
  n ? 971183874599339129547649988289594072811608739584170445
  $ echo 270 | ../../polish.exe -eval fibo.p
  n ? 119447720249892581203851665820676436622934188700177088360
  $ echo 280 | ../../polish.exe -eval fibo.p
  n ? 14691098406862188148944207245954912110548093601382197697835
  $ echo 290 | ../../polish.exe -eval fibo.p
  n ? 1806885656323799249738933639586633513160792578781310139745345
  $ echo 300 | ../../polish.exe -eval fibo.p
  n ? 222232244629420445529739893461909967206666939096499764990979600
  $ echo 310 | ../../polish.exe -eval fibo.p
  n ? 27332759203762391000908267962175339332906872716290689783750745455
  $ echo 320 | ../../polish.exe -eval fibo.p
  n ? 3361707149818144672666187219454104827980338677164658343636350711365
  $ echo 330 | ../../polish.exe -eval fibo.p
  n ? 413462646668428032346940119724892718502248750418536685577487386752440
  $ echo 340 | ../../polish.exe -eval fibo.p
  n ? 50852543833066829834000968538942350270948615962802847667687312219838755
  $ echo 350 | ../../polish.exe -eval fibo.p
  n ? 6254449428820551641549772190170184190608177514674331726439961915653414425
  $ echo 360 | ../../polish.exe -eval fibo.p
  n ? 769246427201094785080787978422393713094534885688979999504447628313150135520
  $ echo 370 | ../../polish.exe -eval fibo.p
  n ? 94611056096305838013295371573764256526437182762229865607320618320601813254535
  $ echo 380 | ../../polish.exe -eval fibo.p
  n ? 11636390653418416980850249915594581159038678944868584489700931605805709880172285
  $ echo 390 | ../../polish.exe -eval fibo.p
  n ? 1431181439314368982806567444246559718305231073036073662367607266895781713447936520
  $ echo 400 | ../../polish.exe -eval fibo.p
  n ? 176023680645013966468226945392411250770384383304492191886725992896575345044216019675
  $ echo 410 | ../../polish.exe -eval fibo.p
  n ? 21649481537897403506609107715822337285038973915379503528404929519011871658725122483505
  $ echo 420 | ../../polish.exe -eval fibo.p
  n ? 2662710205480735617346452022100755074809023407208374441801919604845563638678145849451440
  $ echo 430 | ../../polish.exe -eval fibo.p
  n ? 327491705792592583530106989610677051864224840112714676838107706466485315685753214360043615
  $ echo 440 | ../../polish.exe -eval fibo.p
  n ? 40278817102283407038585813270091176624224846310456696876645445975772848265708967220435913205
  $ echo 450 | ../../polish.exe -eval fibo.p
  n ? 4953967011875066473162524925231604047727791871346061001150551747313593851366517214899257280600
  $ echo 460 | ../../polish.exe -eval fibo.p
  n ? 609297663643530892791951979990217206693894175329255046444641219473596270869815908465388209600595
  $ echo 470 | ../../polish.exe -eval fibo.p
  n ? 74938658661142424746936931013871484819301255773627024651689719443505027723135990224027850523592585
