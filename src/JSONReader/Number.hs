module JSONReader.Number (extractNumber) where


import           Control.Arrow   (first)
import           Data.Char       (isDigit, isSpace)
import           JSONDat
import           JSONReader.Util (plainJNumber, shorten)

extractNumber :: String -> Either String (JDat, String)
{-^
解析JSON数字。按照 @ECMA-404@ 标准，一个JSON数字是一个十进制数，
数字之前不能有多余的0和符号（即 @+@），不接受隐式浮点数（见下例），
接受浮点数和科学计数。下面是不合法的数字：

                      010 0xFF +1 .5 1000_000 1.0e1.0

下面是合法的数字

                    0 255 -10 -0 1.2 1e10 1.2E-010 0e0

上面最后两个数字尽管看上去有问题，但确实是在定义上合法的，并且 @0e0@
的值等于 @0.0@。详细规则参见 @ECMA-404@ 标准。考虑到数值表示的多样性，
包括科学计数的“e”既可以大写也可以小写，数值在 'JDat' 中将以字符串存放。
下面是解析数字的详细规则，约定 @$d@ 代表 0-9 之间任意一个字符，
@$(1..9)@ 代表 1-9 之间任意一个字符， @$!@ 代表能够确定本次数字解析结
束的字符。下面的定义如果有不严谨的地方，可以参考对应的函数。

* 【步骤 A】 一个数字应以符号 - 或 @$d@ 开始              （'stepA'）
如果一个数字以0开始，则下一个字符一定是 [. (e|E) @$!@] （并分别进入步
骤C、D和E）；如果一个数字以 - 开始，其后一定是一个 0 （进入步骤A）或者
@$(1..9)@ （进入步骤B）；如果数字以 @$(1..9)@ 开始，进入步骤B。

* 【步骤 B】 @$d@ 的后面可以是 @$d@                       （'stepB'）
如果进入该步骤，下一个字符可以是 @$d@，并进入步骤B。当遇到 [. (e|E) @$!@]
则分别进入步骤C、D和E。

* 【步骤 C】 . 之后应为 @$d@                              （'stepC'）
字符“.”之后是小数部分，这部分可以是 @$d@，下一个字符可以是 @$d@ 并进入
步骤C。当遇到 [(e|E) @$!@] 则进入步骤D或者步骤E。

* 【步骤 D】 e|E 之后为 [+ - @$d@]                        （'stepD'）
如果出现字符“e”或者“E”，则之后的部分会被解析为科学计数部分。“e”或者“E”
之后可以紧跟一个字符“+”或者“-”，然后进入步骤 D'；也可以直接跟一个 @$d@，
并进入步骤 D'。

* 【步骤 D'】 @$d@ 循环                                   （'stepD\''）
该步骤和步骤B相似，区别在于 @$d@ 之后必须紧跟一个 @$d@，其余任何字符都
会导致解析结束，进入步骤E

* 【步骤 E】 结束解析

其中每一步后续的字符必须为描述到的字符，任何其余字符都会导致解析的结束
（或者失败）。定义中的 @$!@ 为一组可能的分隔符（见 'isTerminate'），如
果出现这些字符，并且当前解析处于可终止的地方，则正常终止解析；如果出现
未知的字符和在不可终止的地方出现了终止字符，都会返回一个错误。就像上面
提到过的，这个解析器只会将数字收集起来，而不在意实际的数值是多少，因此
最大化保证了数值不被破坏；而实际上正确收集到的字符序列也可以用
*Haskell*的相关解析函数转化为实际值。

最后，该函数约定字符流的第一个字符应当为“-”或“0..9”之一，否则返回错误。

-}
extractNumber = either Left (Right . first plainJNumber) . stepA


type Step = String -> Either String (String, String)

stepA, stepB, stepC, stepD, stepD' :: Step

stepA ('0':'.':r) = first ("0."++) <$> stepC r
stepA ('0':c:r) | c `elem` ['e', 'E'] = first (['0', c]++) <$> stepD r
stepA ('0':r)   | isTerminate r       = Right ("0", r)
                | otherwise           = Left $ "在0之后发现非法序列" ++ shorten r

stepA ('-':r@('0':_))           = first ('-':) <$> stepA r
stepA ('-':r@(c:_)) | isDigit c = first ('-':) <$> stepB r
stepA r@('-':_)                 = Left $ "非法的数值结构" ++ shorten r

stepA r@(c:_) | isDigit c = stepB r

stepA _ = Left commonErrmsg


stepB s₀ = case span isDigit s₀ of
  ([], _)                         -> Left commonErrmsg
  (s₁, '.':r)                     -> first ((s₁++['.'])++) <$> stepC r
  (s₁, c:r) | c `elem` ['e', 'E'] -> first ((s₁++[c])++) <$> stepD r
  (s₁, r)   | isTerminate r       -> Right (s₁, r)
  (_ , r)                         -> Left $ illegalCharErrmsg ++ shorten r


stepC s₀ = case span isDigit s₀ of
  ([], r)   -> Left $ "小数点之后应为数字，" ++ shorten r ++ "为非法结构"
  (s₁, c:r) | c `elem` ['e', 'E'] -> first ((s₁++[c])++) <$> stepD r
  (s₁, r)   | isTerminate r       -> Right (s₁, r)
  (_ , r)   -> Left $ illegalCharErrmsg      ++ shorten r


stepD s@(c:r) | c `elem` ['-', '+'] = first (c:) <$> stepD' r
              | isDigit c           = stepD' s
stepD r = Left commonErrmsg


stepD' s₀
  = case span isDigit s₀ of
      ([], r) -> Left $ "科学计数法部分必须包含数字，" ++
                 shorten r ++ "为非法结构"
      (s₁, r) | isTerminate r -> Right (s₁, r)
      _                       -> Left commonErrmsg


isTerminate :: String -> Bool
isTerminate []    = True
isTerminate (c:_) = isSpace c || c `elem` sepList
  where sepList = [',', '}', ']', '/']

commonErrmsg      = "数字解析错误"
illegalCharErrmsg = "解析数字时发现非法字符序列"
