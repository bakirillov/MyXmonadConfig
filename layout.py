import os

with open("/home/bakirillov/.xmonad/current_layout.txt", "r") as ih:
    current_layout = ih.read()
with open("/home/bakirillov/.xmonad/current_layout.txt", "w") as oh:
    if current_layout == "us":
        oh.write("ru")
        os.system("setxkbmap -layout ru")
    else:
        oh.write("us")
        os.system("setxkbmap -layout us")

