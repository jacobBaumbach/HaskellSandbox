module Phone where



data DaPhone = DaPhone
     --"1"
     --"abc"
     --"def"
     --"ghi"
     --"jkl"
     --"mno"
     --"pqrs"
     --"tuv"
     --"wxyz"
     --" "
     --".,"


convo :: [String] 
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]


type Digit = Char
type Presses = Int

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = foldr (\(a,b) -> \acc -> b+acc) (0::Presses) x