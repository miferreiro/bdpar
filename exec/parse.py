#
# Bdpar provide a tool to easily build customized data flows to pre-process
# large volumes of information from different sources. To this end, bdpar allows
# to (i) easily use and create new functionalities and (ii) develop new data
# source extractors according to the user needs. Additionally, the package
# provides by default a predefined data flow to extract and preprocess the most
# relevant information (tokens, dates, ... ) from some textual sources (SMS,
# email, YouTube comments).
#
# Copyright (C) 2020-2022 Sing Group (University of Vigo)
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/gpl-3.0.html>

import email.parser
import email.utils
import sys

def parseElement(filename,element,type):
	parser = email.parser.Parser()
	email_val = parser.parse(open(filename,"r"))
	element_val=None

	if element.lower()=="message":

		if email_val.is_multipart():
			for part in email_val.walk():
				ctype = part.get_content_type()
				cdispo = str(part.get('Content-Disposition'))
				
				#skip any text/plain (txt) attachments
				if ctype == 'text/html' and 'attachment' not in cdispo:
					element_val = part.get_payload(decode=True) #decode
					break
		#not multipart -i.e. plain text, no attachments, keeping fingers crossed
		else:
			element_val = email_val.get_payload(decode = True)
	else:
		element_val=email_val.get_all(element)[0]
		
		
	if element_val!=None:
		print(element_val.decode())
	else:
		print ("".decode())


parseElement(sys.argv[1],sys.argv[2],sys.argv[3])
