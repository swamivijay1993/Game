library(combinat)
library(RGtk2)

a <- c("alex","bob","charles","danny","erik","fred","george")
perpetrators <- sample(a,3)
assign("perp", perpetrators, envir = .GlobalEnv)
perpBoolean = c(F,F,F)
assign("perpBool", perpBoolean, envir = .GlobalEnv)
counter = 0
assign("count", counter, envir = .GlobalEnv)
playerBoolean = c(T,T,T)
assign("playerBool", playerBoolean, envir = .GlobalEnv)

main <- gtkWindow()
main["title"] <- "Three of Crime Game"

mainframe <- gtkFrameNew("GameRules")
main$add(mainframe)

Vbox <- gtkVBoxNew(spacing = 10)
Vbox$setBorderWidth(30)
mainframe$add(Vbox)


desc = gtkLabelNewWithMnemonic("Three-of-a-crime is a simple logic game for up to 3 players. There are 7 different criminals. The computer
randomly chooses three of these (the 'perpetrators'), but doesn't tell the players which are chosen. 
The computer then puts down three random criminals. 0, 1, or 2 of these may be the actual perpetrators. The
computer also tells the player how many (but not which) of the three criminals are perpetrators. The players
may either guess which three criminals are the actual perpetrators or they may pass. If a player guesses
wrong, she is out of the game and the other players continue. If no player chooses to guess, the computer
puts down another three randomly chosen criminals (0, 1, or 2 of which may be actual perpetrators) and
tells the players how many (but not which) of these are actual perpetrators. Players can again use logic to
deduce the three actual criminals.


Note : The seven names are ALEX,BOB,CHARLES,DANNY,ERIK,FRED,GEORGE
In each Set of options ONE is a perpetrators") 
Vbox$packStart(desc)

textchoice<- gtkEntryNew() #text field with choice of player
textchoice$setWidthChars(25)
Vbox$packStart(textchoice)

model<-rGtkDataFrame(c("Player1","Player2","Player3"))
combobox <- gtkComboBox(model)
#combobox allowing to decide whether we want result as integer or double

crt <- gtkCellRendererText()
combobox$packStart(crt)
combobox$addAttribute(crt, "text", 0)

gtkComboBoxSetActive(combobox,0)
Vbox$packStart(combobox)

start <- gtkButton("try")
Vbox$packStart(start,fill=F) #button which will start calculating
gSignalConnect(start, "clicked", ThreeGameGui)


restart <- gtkButton("pass")
Vbox$packStart(restart,fill=F) #button which will start calculating
gSignalConnect(restart, "clicked", restartGame)


ThreeGameGui <- function(button)
{
  count <- count + 1
  assign("count",count,envir = .GlobalEnv)
  print(count)
  
  if(count==1)
  {
    window <- gtkWindow()
    window["title"] <- "Three of Crime"
    
    frame <- gtkFrameNew("Game")
    window$add(frame)
    
    
    box1 <- gtkVBoxNew(spacing = 10)
    box1$setBorderWidth(30)
    frame$add(box1)   #add box1 to the frame
    
    choice1label = gtkLabelNewWithMnemonic("Choice 1") #text label
    box1$packStart(choice1label)
    
    choice2label = gtkLabelNewWithMnemonic("Choice 2") #text label
    box1$packStart(choice2label)
    
    choice3label = gtkLabelNewWithMnemonic("Choice 3") #text label
    box1$packStart(choice3label)
    
    
    a<- choices(a)
    choice1label$setText(a[1])
    choice2label$setText(a[2])
    choice3label$setText(a[3])
    
  }
  
  else
  {
    text <- textchoice$getText()
    player <- gtkComboBoxGetActive(combobox)+1
    
    if(text == "")
    {
      dialog <- gtkDialog("Message", NULL, "destroy-with-parent",
                          "gtk-ok", GtkResponseType["none"],
                          show = FALSE)
      label <- gtkLabel("Enter One of the Person or click pass")
      gSignalConnect(dialog, "response", gtkWidgetDestroy)
      dialog[["vbox"]]$add(label)
      dialog$showAll()
    }
    
    else 
    {
      if(playerBool[player]==T)
      {
      if(!(all((perpBool == c(T,T,T)) == T)))
      {
        bool <- perpBool
        found <- F
        for(i in 1:length(perp))
        {
          if(text == perp[i])
          {
            bool[i] <- T
            assign("perpBool",bool,envir = .GlobalEnv)
            found <- T
            dialog <- gtkDialog("Message", NULL, "destroy-with-parent",
                                "gtk-ok", GtkResponseType["none"],
                                show = FALSE)
            label <- gtkLabel(paste(text,"is a perp",sep=" "))
            gSignalConnect(dialog, "response", gtkWidgetDestroy)
            dialog[["vbox"]]$add(label)
            dialog$showAll()
          }
        }
        print(found)
        
        if(found==F)
        {
          pb <- playerBool
          pb[player] <- F
          assign("playerBool",pb,envir = .GlobalEnv)
          print(playerBool)
        }
        
        if(!(all((perpBool == c(T,T,T)) == T)))
        {
          window <- gtkWindow()
          window["title"] <- "Three of Crime"
          
          frame <- gtkFrameNew("Game")
          window$add(frame)
          
          
          box1 <- gtkVBoxNew(spacing = 10)
          box1$setBorderWidth(30)
          frame$add(box1)   #add box1 to the frame
          
          choice1label = gtkLabelNewWithMnemonic("Choice 1") #text label
          box1$packStart(choice1label)
          
          choice2label = gtkLabelNewWithMnemonic("Choice 2") #text label
          box1$packStart(choice2label)
          
          choice3label = gtkLabelNewWithMnemonic("Choice 3") #text label
          box1$packStart(choice3label)
          
          
          a<- choices(a)
          choice1label$setText(a[1])
          choice2label$setText(a[2])
          choice3label$setText(a[3])
        }
        else
        {
          dialog <- gtkDialog("Message", NULL, "destroy-with-parent",
                              "gtk-ok", GtkResponseType["none"],
                              show = FALSE)
          label <- gtkLabel("All perpetrators are found!! Game is over")
          gSignalConnect(dialog, "response", gtkWidgetDestroy)
          dialog[["vbox"]]$add(label)
          dialog$showAll()
        }
      }
      }
      else
      {
        dialog <- gtkDialog("Message", NULL, "destroy-with-parent",
                            "gtk-ok", GtkResponseType["none"],
                            show = FALSE)
        label <- gtkLabel("The Player is Out of Game")
        gSignalConnect(dialog, "response", gtkWidgetDestroy)
        dialog[["vbox"]]$add(label)
        dialog$showAll()
      }
    }
    
  }
}


#Function to make combinations
choices <- function (vector)
{
  perm <- permn(sample(vector,3))
  count <- 0
  while(count==0)
  {
  for(i in 1:length(perm))
  {
    if((length(intersect(perm[[i]],perpetrators)))==1)
    {
      count <- 1
      val <- i
    }
    else
    {
      perm <- permn(sample(vector,3))
    }
  }
  }
  return(perm[[val]])
}

restartGame <- function(button)
{
  window <- gtkWindow()
  window["title"] <- "Three of Crime"
  
  frame <- gtkFrameNew("Game")
  window$add(frame)
  
  
  box1 <- gtkVBoxNew(spacing = 10)
  box1$setBorderWidth(30)
  frame$add(box1)   #add box1 to the frame
  
  choice1label = gtkLabelNewWithMnemonic("Choice 1") #text label
  box1$packStart(choice1label)
  
  choice2label = gtkLabelNewWithMnemonic("Choice 2") #text label
  box1$packStart(choice2label)
  
  choice3label = gtkLabelNewWithMnemonic("Choice 3") #text label
  box1$packStart(choice3label)
  
  
  a<- choices(a)
  choice1label$setText(a[1])
  choice2label$setText(a[2])
  choice3label$setText(a[3])
}

