def wordchoice(x): #defines function wordchoice
    import random #imports module
    global selection #assigns selection as a global variable
    global coded #assigns coded as a global variable
    selection=random.choice(wordlist) #chooses a random item from the list of words
    selection=selection.strip() #removes any spaces before or after the word
    coded=str() #creates emptry string coded
    for ch in selection:
        coded=coded+'-' #loops through the word and replaces the letters with dashes
    print('Here is your word:\n') #lets the user know that the following display of dashes represents their word
    print('\n' +coded) #prints the list of dashes that signifies the chosen word

def guess(x): #defines function guess
    incorrect=0 #establishes variable incorrect as 0
    global coded #assigns coded as a global variable
    global letter #assigns letter as a global variable
    global selection #assigns selection as a global variable
    global wins #assigns wins as a global variable
    chlist=list(selection) #creates a list of the chosen word
    listcoded=list(coded) #creates a list of the word in dashes
    while listcoded != chlist:
        if letter.lower() in chlist:
            y=chlist.count(letter.lower()) #counts how many times the letter is in the word
            x=0 #assigns value 0 to the variable x
            while y >= 1:
                for ch in chlist:
                    if ch == letter.lower(): #loops through the characters in the word to determine if the character is the same as the user's letter choice
                        i=int(chlist.index(ch))+x #assigns i to the index operator of the character in the word
                        chlist.remove(ch) #removes the character from the word's list
                        y-=1 #removes 1 from y
                        listcoded[i]=letter.lower() #assigns the letter to the comparable slot in the list of dashes
                        x+=1 #adds 1 to x
            chlist=list(selection) #assigns chlist as the list of the chosen word
            coded=str() #assigns coded as an empty string
            for item in listcoded:
                coded=coded+item #transfers letters from list form to string form
            print(coded) #prints the updated dash-letter form of the chosen word
            if listcoded == chlist: #compares the coded form to the original word form
                print('Congratulations! You have discovered the secret word.') #prints a success message if the words are the same
                wins+=1 #adds 1 to variable wins
                break #breaks the loop
        else:
            print('Sorry! That letter is not in your word.') #alerts the user that the chosen letter is not in the selected word
            incorrect=incorrect+1 #adds 1 to variable incorrect
            print('You have made ' + str(incorrect) + ' wrong guesses.  You have '+ str(8-incorrect) +' guesses left.') #alerts the user to the number of incorrect guesses they have made and have remaining
            if incorrect == 8: #determines if the user has exhausted their incorrect guesses
                print('Sorry! You have run out of guesses. The word was ' + selection +'.') #alerts the user that they are out of tries
                break #breaks the loop
        letter=' '
        letter=input('Please guess a letter. \n') #asks the user to guess a new letter
        
            
    

print('Welcome to Hangman!') #Greets the user
print('''Here are the rules of the game:

1. You will be presented with a row of dashes.  Each dash is a place holder for one letter in a randomly chosen word.
2. You must choose a letter from the English alphabet.
3. If the letter is in the word, it will appear in it\'s correct position(s) in the word.  If not, it will be counted as an incorrect guess.
4. You are only allowed 8 incorrect guesses per game.

Let\'s play!\n''') #Notifies the user of how the game is played and what the rules are

infile=open('dictionary.txt') #opens the dictionary and assigns to infile
words=infile.readlines() #reads each line of the dictionary
global wordlist #establishes wordlist as a global variable
wordlist=[] #creates new list wordlist
for word in words:
    wordlist.append(word) #adds words to wordlist
tries=0 #assigns value 0 to variable tries
global wins #establishes wins as a global variable
wins=0 #assigns value 0 to variable wins
nextstep='Y' #assigns the letter Y to variable nextstep
while nextstep.upper() == 'Y':
    wordchoice(wordlist) #initiates function wordchoice
    letter=input('Please guess a letter. \n') #prompts user to guess a letter
    guess(letter) #initiates function guess
    tries+=1 #adds 1 to tries
    nextstep=' '
    nextstep=input('Would you like to play again? Please enter Y or N.  ') #asks user if they would like to play again
if nextstep.upper() == 'N':
    print('You played ' + str(tries) + ' rounds and won ' + str(wins) + ' times.') #tells user their session statistics
else:
    print('Your entry was not valid.  Please enter Y or N.') #informs the user that they did not enter a Y or N
    nextstep=' '
    nextstep=input('Would you like to play again? Please enter Y or N.  ') #reprompts user for a valid entry
