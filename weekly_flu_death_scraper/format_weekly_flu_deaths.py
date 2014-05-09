import re
wd = '/Users/jj/flu_surveil_data/'

input = open(wd +'mmwr_formatted.csv','r')
output = open(wd + 'mmwr.csv','w')

line = input.readline()
while(line):
    print "old line: " + line 
    line = re.sub(r'-','0',line)
    line = re.sub(r'U,','NA,',line)
    print "new line: " + line
    output.write(line)
    line = input.readline()
    
input.close()
output.close()