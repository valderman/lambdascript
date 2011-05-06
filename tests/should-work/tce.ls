smashStack 0 = 0;
smashStack n = smashStack (n-1);

main = smashStack 10000;
