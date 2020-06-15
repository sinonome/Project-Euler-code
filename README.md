# README
このリポジトリは、 project euler を関数型言語で解いた際のソースコードを格納している場所です。
project euler のネタバレが含まれますので閲覧には十分注意してくだいますようよろしくお願いします。

# 今後の予定
Rust とか F# とかで問題を解くかも...(OCaml でこれ解けるのか？？？って思う問題が出てきたため)

# _before ファイルについて
この ml ファイルは、その問題を解く際に書いたソースコードで、実行したら正解の答えが出力されるようになっています。
しかし、そのファイルは実行すると多くの時間を必要とします。
なので、このソースコードとは別により実行時間が短いソースコードを作成しています。

つまり、これ実行したら時間かかるし別の方法で解こう、でもこのファイル残しておきたいから _before つけとけ、
くらいのノリで push しています。

# 各問題に対する言及
## 問10について
この問題、要するに200万までの素数をとりあえず列挙しろって言ってるんですけど、
これは次のような方法で列挙することが考えられます。

1. 素数判定機を作ってfor文でガンガン回す
2. エラトステネスの篩を使う

1については言及の余地がないんで、まあこれを実装すればいいんですが、実行時間が約 10s と
かなり時間を要してしまいます。今回はさらに200万回ループしないといけないのでなおさらです。
なので、2を採用したいと思いました。しかし、範囲が100とかならまだしも、今回は200万。
listの作成時に stack overflow だよと怒られてしまいました。
これは200万までの値を格納する list を作成する際にエラーをはいていて、そうなってしまうと、
別の手段で list を作る必要があるのですが、その手段がわからない状態です。
なので、諦めて1を実装してこの問題を終えることにしました。
そもそも2をリストつかって実装するのは早いかと言われればそうとはいえないと思っていて、
2を実装しても早くなる見込みが少なかったので。ここらへんの話について、誰か詳しい方いましたら教えてください...

ちなみに、Python で numpy 使ってエラトステネス実装したら 0.1s かかるかかからないかくらいで処理を終えることができるんですね。
numpy すごいな...