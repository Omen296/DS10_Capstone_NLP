################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(predict.baseline, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
