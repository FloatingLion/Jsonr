module JR () where



import           JSONReader



_p :: String -> IO ()
_p s = putStrLn $ case jread s of
  Left msg -> "错误：" ++ msg
  Right v  -> show v

s₁ = "{ \"people\": [\n\
\  { \"firstName\": \"Brett\", \"lastName\":\"McLaughlin\", \"email\": \"brett@newInstance.com\" },\n\
\  { \"firstName\": \"Jason\", \"lastName\":\"Hunter\", \"email\": \"jason@servlets.com\" },\n\
\  { \"firstName\": \"Elliotte\", \"lastName\":\"Harold\", \"email\": \"elharo@macfaq.com\" }\n\
\ ]}"
