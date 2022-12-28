## Strat 

print("Hello Guys")
print("Welcome to Pao Ying Chub!")
gaming <- function() {

## Create variables

tool <- c("r","s","p")
num_match <- 0
wins <- 0
draws <- 0
loses <- 0

## Craete Bot 
  
bot <- function(){
  bot <- sample(tool,size=1,replace = TRUE)
}

## Optionals to play

print("Are you Ready!")

while (num_match > -1){
  print("Please press 'Yes' for start game or preess 'ex' for exit the game")
  quit <- readLines ("stdin", n = 1)
  if(quit == "ex"){
    break
  }else{
    print("Let go!")
  }

## Your Selection 

print("Please select your weapon: 'r' for Rock, 'p' for Paper & 's' for Scissors.")
you_chose <- readLines ("stdin", n = 1)

if ((you_chose == tool[1]) | 
    (you_chose == tool[2]) | 
    (you_chose == tool[3])) {
    print("Great you already selected")
    } else {
      ("----------------")
      print("Wrong input.Please run game again")
      break
    }

## opponent action

    opp_tool <- bot()
    print(paste("Opponent:",opp_tool))
    
## number of matches

    num_match <- num_match + 1

##  if Wins

  if(you_chose==tool[1] & opp_tool==tool[2]|
     you_chose==tool[2] & opp_tool==tool[3] |
     you_chose==tool[3] & opp_tool==tool[1]){
    print("Congratulations! you are the winner.")
    wins <- wins+1
    
## if draw

     }else if(you_chose == opp_tool){
    print("So close! you are draw,but Let's try again")
    draws <- draws+1
     }else{
     
## if lose

    print("Oh! you lose,but Let's try again ")
    loses <- loses +1 
     }
  }

win_rate_percen <- (wins/num_match)*100
##Summarize
print("----------------")
  score <- data.frame(
                      No.Match = num_match,
                      Win=wins,
                      Drwas=draws,
                      Loses=loses,
                      Win_Persentage= win_rate_percen)
  print(score)
  print("----------------")
  
  # Goodbye!
  if(num_match > 0) {
    print("Let's play again next time.See you!")
  } 
}

gaming()
