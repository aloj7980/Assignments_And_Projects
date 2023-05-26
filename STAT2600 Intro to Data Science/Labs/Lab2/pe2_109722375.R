index = 1
oneBehind = 0
twoBehind = 0
sum = 0
while (index <= 4000000){
  if(index %% 2 == 0){
    sum = sum + index
  }
  twoBehind = oneBehind
  oneBehind = index
  index = oneBehind + twoBehind
}
print(sum)
