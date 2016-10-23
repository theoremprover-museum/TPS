{for(i=1;i<=width;i= i + colwidth)
     {print(substr($0,i,colwidth))>>(newfile ("." ((1 + ((i - 1) / colwidth)) "")))}}

