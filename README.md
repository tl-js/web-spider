# web-spider
After seeding with a list of relevant URLs, and a download limit (# pages) the spider queues them up. Then the following algorithm is executed :

    (while (and (queue-not-empty) (limit-not-reached)
      Pop the queue (returns a URL)
      Fetch the source at the URL
      Extract title from the page ---> Page_Title
      Extract valid URLs from the page
      Queue up the newly found URLs
    )

Following commands to be used in DrRacket to run the spider
        (seed  '("http://www.RelevantURL.com" "http://www.AnotherOne.com")

        (crawl x) ; x is the maximum number of pages you wish to download
