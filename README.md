# Steve Haroz's analysis of the calcChain file from DataColada post 109

The original post is here: https://datacolada.org/109

I noticed that DataColada's analysis of calcChain, which shows the order of Excel calculations, was not included in their analysis and was likely done manually. So I analyzed it programmatically and got identical results. 

I classify a row as in-order if any of the following are true:
* the calcChain order is one more than the previous row
* the calcChain order is one less than the next row
* the calcChain order is both bigger than the previous row and smaller than the next row
  
Otherwise, it's flagged as out of order.

DataColada chart:  
![image](https://github.com/steveharoz/data_colada_109/assets/2257540/d4907851-ab02-4af9-9534-85982efe0a0f)

My reanalysis:  
![image](output/steve%20plot.png)
