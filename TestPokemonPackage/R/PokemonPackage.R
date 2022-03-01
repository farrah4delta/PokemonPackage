#' Data about Pokemon
#'
#' a dataset with data about pokemon from source
#'
#' @format A dataframe wit 1076 number of rows/observations and 47 variables
#' \describe {
#' \item{Pokemon.Id}{ID number}
#' \item{Pokedex.Number}{Pokemon's number in the PokeDex}
#' \item{Pokemon.Name}{name of the pokemon}
#' \item{Classification}{a description of the pokemon}
#' }
#' @source Kaggle
"pokemon_data"

# for all functions, you can return values based on the first or random record if there are multiple matches,

# 1.  Write a function to print some information about a pokemon, Print its name, classification, and primary and secondary types
#' Function for getting data about a single pokemon
#' @param pokemon name of the pokemon we want information about
#' @examples pokeDex("Charmander")
#' @examples pokeDex("Litwick")
#' @return None
#' @export pokeDex
pokeDex<-function(pokemon){
  return(pokemon_data[pokemon_data$Pokemon.Name==pokemon, c(3, 4, 10, 11)][1,]) #choose first one if has multiple matching rows
}
#each number represents which column

# 2. Write a function that takes in 2 pokemon and determines if the attack stat of the first pokemon is higher than the defense stat of the second.

#' Function for battling pokemon
#' @param attacker name of the attacking pokemon
#' @param defender name of the defending pokemon
#' @examples atkCheck("Charmander","Urshifu" )
#' @return None
#' @export
atkCheck<-function(attacker, defender){
  if(pokemon_data[pokemon_data$Pokemon.Name==attacker, ]$Attack.Stat[[1]] > pokemon_data[pokemon_data$Pokemon.Name==defender, ]$Defense.Stat[[1]]){
    return(paste(attacker, "wins"))
  }else{
    return(paste(defender, "wins"))
  }
}

# 3. Write a function that returns the result of trying to catch the pokemon at a particular health
# using a modification of the formula found below catchProb=((3 * HPmax - 2 * HPcurrent) * ratemodified / (3 * HPmax))/100
#from here: https://bulbapedia.bulbagarden.net/wiki/Catch_rate#General_capture_method_.28Generation_II_onwards.29
#calculate the percent likelihood then sample c(TRUE,FALSE) with probabilities c(catchProb,1-catchProb) and return the result
#get the HPmax from the data

#' Function to see if you catch the pokemon or not
#' @param pokemon name of the pokemon we want to catch
#' @param HPcurrent current health of pokemon
#' @examples HPcurrent("Charmander", 20 )
#' @return None
#' @export
pokeBall<-function(pokemon, HPcurrent){
  HPmax <-pokemon_data[pokemon_data$Pokemon.Name==pokemon, ]$Health.Stat[[1]]
  ratemodified <-pokemon_data[pokemon_data$Pokemon.Name==pokemon, ]$Catch.Rate[[1]]
  catchProb <-((3 * HPmax - 2 * HPcurrent) * ratemodified / (3 * HPmax))/100
  catchStatus<-sample(c(T, F),size=1, prob=c(catchProb,1-catchProb),replace=T)
  return(catchStatus)
}

#R6 class

library(R6)
pokemon = R6Class(
  "pokemon",  #name of the class
  public= list( #list of publicly available fields (attributes) for the class
    name = NULL,
    type1 = NULL,
    type2 = NULL,
    startingHP = NULL,
    awake = NULL,
    initialize = function(name){
      data("pokemon_data")
      if(name %in% pokemon_data$Pokemon.Name){ #index into row, create variable for that row, index by going into the variable by its name
      row = pokemon_data[pokemon_data$Pokemon.Name == name,]
      self$name = name
      self$type1 = row[1, "Primary.Type"]
      self$type2 = row[1, "Secondary.Type"]
      self$startingHP = row[1, "Health.Stat"]
      self$awake = TRUE
      self$show()
      }else{
        print("This Pokemon does not exist.")
      }
    },
    show = function(){
      cat(self$name, ": ", self$type1, self$type2, self$startingHP, "awake: ", self$awake)
    },
    changeHP = function(damage){
      HP = self$startingHP
      self$startingHP = HP - damage
      if(HP <= 0){
        self$awake = FALSE
      }
      print(HP)
    },
    fight = function(opponent){ #checks if pokemon is awake #calls changeHP(), finds pokemon to fight,
      while(self$awake==TRUE & opponent$awake == TRUE){
        opponent$changeHP(7)
        self$changeHP(5)
        cat("self: ", self$startHP, "opponent: ", opponent$startingHP) #use paste() or print()?
      }
    }

    )
  )

#' @format This R6 class represents a pokemon
#' @source Kaggle
#'
# Then
#1.  create a package with the data set and functions
#2.  roxygen notes, generate the documentation,
#3.  install the package
#4.  view your documentaion with ?pokeDex etc
#5.  use your functions
