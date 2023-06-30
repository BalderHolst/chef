import requests

resp = requests.get("https://gist.githubusercontent.com/Bilka2/6b8a6a9e4a4ec779573ad703d03c1ae7/raw")

if resp.status_code != 200:
    print("Could not download data.raw")
    exit(1)

text = resp.text
text = text.replace("=", ":")

print(text)

