# Python Script to Download the most upvoted image on certain subreddits to directory on the computer.

import os, requests, json, time, datetime, threading, re 

# Add i and .jpg to imgur.com url's
def add_ImgurExt(url):
	return url[0:7] + 'i.'+url[7:]+'.jpg'

# Function to remove illegal characters for filenames and replace them with spaces
def remove(title):
	illegalChar = ['<','>',':','\"','\\','/','|','?','*']
	newTitle = ''
	for letter in title:
		if letter in illegalChar:
			newTitle += ' '
		else:
			newTitle += letter
	return newTitle

# Function to get JSON off a subreddit. Make sure subreddit exists
def get_RedditJSON(subreddit):
	#Sometimes reddit request returns Error 429. 
	try:
		res = requests.get('https://www.reddit.com/r/{0}.json'.format(subreddit))
		res.raise_for_status()
		return json.loads(res.text)
	#Recall function if json is not received.
	except requests.HTTPError:
		print('Error: 429, requesting again', end ='\n\n')

# Function to Get Image Link from Flickr
def get_FlickrImage(flickrLink):
	imageHTML = requests.get(flickrLink)	

	# Find image link in source.
	imageURL = re.findall(r"""
				farm
				[^":]*
				_[o|k|h|b]\.
				[jpg|png|gif]*
				""", imageHTML.text, re.VERBOSE)[0]
	imageURL = 'https://' + imageURL
	imageURL = imageURL.replace('\\','')
	return imageURL

# Function to download pictures off any JSON Reddit page
def download_RedditJSON(json):
	for i in range(len(json['data']['children'])):
		name = json['data']['children'][i]['data']['title'] + '.jpg'
		name = remove(name)

		print('Saving: ', name.encode("utf-8"))

		# Check if file is already downloaded.
		if name not in images:
			imageURL = json['data']['children'][i]['data']['url']
			
			# Change imgur links that only link to website and not actual image.
			if 'imgur' in imageURL and 'i.imgur' not in imageURL:
				imageURL = add_ImgurExt(imageURL)
			if 'flickr' in imageURL and not imageURL.endswith('.jpg'):
				imageURL = get_FlickrImage(imageURL)
			try:
				imageJPEG = requests.get(imageURL)
				imageJPEG.raise_for_status()

				# Download only for files with .jpg extension 
				if imageURL.endswith('.jpg'):
					with open(name, 'wb') as f:
						for chunk in imageJPEG.iter_content(100000):
							f.write(chunk)
					print('Successfullly saved file!')
				else:
					print(imageURL)
					print('No .jpg link provided')

			except requests.exceptions.ConnectionError:
				print('Connection to Image Site Timed Out')
			except requests.HTTPError:
				print('Too many requests')

		else:
			print('Image already exists!')
		print()
		print(''.center(80,'-'), end = '\n')		

# Fucntion to create thread for subreddit that begins download
def createThread(subreddit):
	json = get_RedditJSON(subreddit)

	# Occasionaly, Reddit returns None for JSON. Wait and call again.
	while json == None:
		time.sleep(3)
		json = get_RedditJSON(subreddit)

	thread = threading.Thread(target = download_RedditJSON, args = [json])
	thread.start()
	return thread


# Print start time.
startTime = time.time()
startTimeP = datetime.datetime.fromtimestamp(startTime).strftime('%Y/%m/%d %H:%M:%S')
print('\n\n')
print(('Starting download at: '+ startTimeP).center(80, '*'), end= '\n')

# Setup place to download files to.
os.makedirs('C:\\Users\\Leo\\Pictures\\Earth and Village Porn', exist_ok = True)
os.chdir('C:\\Users\\Leo\\Pictures\\Earth and Village Porn')
images = os.listdir('.')

# List of subreddits to download.
subreddits = []
subreddits.append(createThread('earthporn'))
subreddits.append(createThread('villageporn'))

# Wait for all threads to finish.
for thread in subreddits:
	thread.join()

# Print how long downloading took.
print('\n')
endTime = time.time()
print(('Downloading took: ' + str(round(endTime-startTime, 2)) + ' seconds.').center(80,'*'))
print('Everthing is saved!'.center(80,'*'))

# Log the download time in text file
logFile = open('Log.txt','a')
logFile.write('\nStarted download at: '+ startTimeP)
logFile.write('\nDownloading took: ' + str(round(endTime-startTime, 2)) + ' seconds.')
logFile.write('\n'+''.center(80,'-'))
logFile.close()

# Set up Task Scheduler so script runs daily
