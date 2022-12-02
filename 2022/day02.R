input <- matrix(scan("input02.txt",""),nc=2,byrow=TRUE)
whowon <- apply(input,1,function(x)switch(x[1],"A"=switch(x[2],"X"=3,"Y"=6,"Z"=0),
                  "B"=switch(x[2],"X"=0,"Y"=3,"Z"=6),
                  "C"=switch(x[2],"X"=6,"Y"=0,"Z"=3)))
sum(whowon+sapply(input[,2],function(x)switch(x,"X"=1,"Y"=2,"Z"=3)))
#9651

whatplayed <- apply(input,1,function(x)switch(x[1],"A"=switch(x[2],"X"=3,"Y"=1,"Z"=2),
                                          "B"=switch(x[2],"X"=1,"Y"=2,"Z"=3),
                                          "C"=switch(x[2],"X"=2,"Y"=3,"Z"=1)))
sum(whatplayed+sapply(input[,2],function(x)switch(x,"X"=0,"Y"=3,"Z"=6)))
#10560