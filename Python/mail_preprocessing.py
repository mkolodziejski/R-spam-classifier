'''
Created on Apr 6, 2011

@author: Marek Skrajnowski
'''

# paths to directories which contain ONLY email names (sub-directories are also supported)
mail_paths = [
              "C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\raw",
              #"C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\easy_ham",
              #"C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\easy_ham_2",
              #"C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\hard_ham",
              #"C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\spam",
              #"C:\\Users\\Marek.Marek-Netbook\\Documents\\Dane MOW\\spam_2"
              ]

overwrite_files = False


import email

import os
from os import path

import re


# normalize paths and remove final slashes (if any)
for mail_path in mail_paths:
    path.normpath(mail_path)
    if mail_path.endswith("/"):
        mail_path = mail_path[:-1]
        
html_expressions = [["<[^>]*>",""],["&gt;",">"],["&lt;","<"],["&quot;","\""],["&nbsp;"," "]]
for expression in html_expressions:
    expression[0] = re.compile(expression[0])
    
def removeHTML(string):
    for expression in html_expressions:
        string = expression[0].sub(expression[1], string)    
    return string

num_expression = re.compile("[0-9]+")
def removeNumbers(string):
    re.sub("","","")
    return num_expression.sub(" ", string)



if __name__ == '__main__':
    
    found_content_types = []
    mail_number = 1
    
    for mail_path in mail_paths:
        # for each mail path walk the directory tree and process each name found
        for root, dirs, names in os.walk(mail_path):
            
            # create an output path for the current directory
            mail_dir_parent, mail_dir_name = path.split(mail_path)
            output_dir = path.join(mail_dir_parent, "%s_out" % (mail_dir_name), root[len(mail_path)+1:])
            os.makedirs(output_dir, exist_ok = True)                
            
            for name in names:
                
                category = "spam" if "spam" in root.lower() else "ham"
                
                # create output path for the current file
                input_path = path.join(root, name)
                output_path = path.join(output_dir, "%s_%06d" % (category, mail_number))
                mail_number += 1
                
                # check if output file already exists and skip if overwrite flag is not set
                if path.exists(output_path):
                    print("File already exists:", output_path)
                    if overwrite_files:
                        print("Overwriting:", output_path)
                    else:
                        print("Skipping:", input_path)
                        continue
                
                # read the email from file
                with open(input_path, mode="r") as input_file:
                    try:
                        mail = email.message_from_file(input_file)
                    except Exception as e:
                        print("Exception for:", input_path)
                        print(e)      
                        print("Skipping:", input_path)                  
                        continue
                    
                # processing
                mail_text = ""
                                
                for part in mail.walk():
                    if part.get_content_type() not in found_content_types:
                        found_content_types.append(part.get_content_type())
                    
                    if part.get_content_type() in ("text/plain",):
                        mail_text += removeNumbers(part.get_payload())
                    elif part.get_content_type() in ("text/html",):
                        mail_text += removeNumbers(removeHTML(part.get_payload()))
                        
                  
                # save the processed mail content to a file in the output path
                if(mail_text != ""):  
                    with open(output_path, mode='w') as output_file:
                        output_file.write(mail_text)
                
                
    print(found_content_types)
    
    