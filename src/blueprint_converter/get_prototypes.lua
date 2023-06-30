local http = require("socket.http")
local body, code = http.request("https://gist.githubusercontent.com/Bilka2/6b8a6a9e4a4ec779573ad703d03c1ae7/raw")
if not body then error(code) end

print(body)
