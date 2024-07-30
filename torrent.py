import re
import requests
from bs4 import BeautifulSoup

	  search_term = "magnet:?xt=urn:btih:"
	  board = "torrents"

	  response = requests.get(f"https://a.4cdn.org/{board}/search.json?q={search_term}")

	  if response.status_code == 200:
		  search_results = response.json()
	# do something with the search results
else:
	print("There was an error performing the search.")

  def way1():
	  # Make a GET request to the 4chan thread URL
	response = requests.get('http://boards.4chan.org/t/thread/1312259')

	# Parse the HTML content of the page
	soup = BeautifulSoup(response.text, 'html.parser')

	# Find all the posts in the thread
	posts = soup.find_all('div', class_='postMessage');

  def way2):
	  response = requests.get(url)
	html = response.text;

	  # Compile the regular expression pattern to match magnet links
	  pattern = r'magnet:\?[^"]+<span class="quote">&gt;magnet:\?xt=urn:btih:(.*)<\/span>'

	  # Find all matches in the HTML
	  matches = re.findall(pattern, html)
	  matches = re.sub(r"<span class="quote">(.*?)<\/span><br>|&gt;|<wbr>|&amp;|&#039;", lambda x: {
		  "&gt;": ">",
		  "<wbr>": "",
		  "&amp;": "&",
		  "&#039;": "'"
		  }[x.group()], string)

	  # validate
	  pattern = r'magnet:\?xt=urn:[a-z0-9]+:[a-zA-Z0-9]{40}&(.*)'

	  # Print the matches
	  for match in matches:
		  print(match)
