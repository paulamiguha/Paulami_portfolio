#Developer : Paulami Guha
#Purpose    : Webscrape vintage musical instrument Seller Data from the Etsy website

import scrapy

filename = 'etsymusicalinstruments.csv'

#Define Etsy record
class EtsyProduct(scrapy.Item):
    SellerName=scrapy.Field()
    CountShopFavs=scrapy.Field()
    TotalSales=scrapy.Field()
    TotalShopReviews=scrapy.Field()
    ShopRating=scrapy.Field()
    YrsActiveWithEtsy=scrapy.Field()
    SellerReturnPolicy=scrapy.Field()
    SellerReturnDays=scrapy.Field()
    ShippingTime=scrapy.Field()
    SellerResponse=scrapy.Field()

class EtsySpider(scrapy.Spider):
    name = "etsy_spider"
    #Starting page : Enter through Etsy Vintage music instrument catalog
    start_urls = [
            'https://www.etsy.com/c/vintage/books-movies-and-music/music/musical-instruments?explicit=1'
            ]
    
    items=[]
    item={}
    nexturl=''
    pagecounter=0
    with open(filename, 'w') as f:
        # Write header to file
        f.write('seller_name,Count_of_favs,total_sales,total_shop_reviews,shop_rating,years_active_with_Etsy,seller_return_policy,seller_return_time,seller_shipping_time,seller_responsivess\n')

        def parse(self, response):  
            self.pagecounter=self.pagecounter+1            
            for href in response.xpath("//*[starts-with(@class, ' display-inline-block listing-link')]/@href[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'),'vintage')]"):      
                # Extract product url
                itemurl = response.urljoin(href.extract())
                print(itemurl)  
                # Extract next page link
                nexturl=response.xpath("//link[@rel='next']/@href")
                if nexturl.extract() == []:
                    self.nexturl=None
                else:
                    self.nexturl=nexturl.extract()[0]
                    self.nexturl = response.urljoin(self.nexturl)
                #Go to product page
                yield scrapy.Request(itemurl, callback=self.parse_item_pages)
           

        def parse_item_pages(self, response):
            # Inside seller page
            item=EtsyProduct()            
            # Seller Name
            sellername=response.xpath("//*[@class='wt-text-link-no-underline']/span/text()")
            sellername=sellername.extract()#.strip()    
            # Seller Page URL
            sellerurl=response.xpath("//*[@class='wt-text-link-no-underline']/@href[contains(.,'simple-shop-header')]")
            sellerurl=sellerurl.extract()[0]
            sellerurl = response.urljoin(sellerurl)
            # Shipping Time
            shippingtime=response.xpath("//*[@class='wt-grid wt-mb-xs-6']/div[1]/p/text()")
            shippingtime=shippingtime.extract()#.strip()
            # This seller responds in
            sellerresponse=response.xpath("//*[@class='wt-text-caption wt-text-center-xs wt-pt-xs-2 wt-text-gray']/b/text()")
            sellerresponse=sellerresponse.extract()
            # Whether seller accepts returns, exchanges or cancellations
            sellerreturnpol=response.xpath(".//div[@data-id='refunds']/div/div[1]/h3[1]/text()")
            sellerreturnpol=sellerreturnpol.extract()
            # seller return policies - scraped from view shop policies link
            sellerreturndays=response.xpath(".//div[@data-id='refunds']/div/div[1]/div[1]/span/text()")
            sellerreturndays=sellerreturndays.extract()
            # Total sales for seller
            totalsales=response.xpath(".//*[@class='wt-text-body-01 mr-xs-1']/text()[contains(.,'sales')]")
            totalsales=totalsales.extract()
            type(sellerresponse)

            if sellername == []:
                sellername=''
            else:
                sellername=sellername[0].strip()
                
            if shippingtime == []:
                shippingtime=''
            else:
                shippingtime=shippingtime[0].strip()

                 
            if sellerresponse == []:
                sellerresponse=''
            else:
                sellerresponse=sellerresponse[0].strip()

                       
            if sellerreturnpol == []:
                sellerreturnpol=''
            else:
                sellerreturnpol=sellerreturnpol[0].strip()

            if sellerreturndays == []:
                sellerreturndays=''
            else:
                sellerreturndays=sellerreturndays[0].strip()

            if totalsales == []:
                totalsales=''
            else:
                totalsales=totalsales[0].strip()
            
            item['SellerName']=sellername
            item['ShippingTime']=shippingtime
            item['SellerResponse']=sellerresponse  
            item['SellerReturnPolicy']=sellerreturnpol  
            item['SellerReturnDays']=sellerreturndays  
            item['TotalSales']=totalsales            
            print("------------------------------")
            Request=scrapy.Request(sellerurl, callback=self.parse_seller_pages)
            Request.meta['item'] = item
            #print(item)
            # Go to seller page
            yield Request
            
        def parse_seller_pages(self, response):
            # Inside seller page
            item = response.meta['item']  
            # Number of years on Etsy
            etsysince=response.xpath(".//*[@class='etsy-since no-wrap']/text()")
            etsysince=etsysince.extract()
            # Total reviews for seller
            totalreviews=response.xpath("//*[@class='display-inline-block vertical-align-middle']/text()")            
            totalreviews=totalreviews.extract()
            # Total reviews for seller
            shoprating=response.xpath(".//input[@type='hidden' and @name='initial-rating']/@value")            
            shoprating=shoprating.extract()
            # Count of admirers (marked as favorites) Dependent variable
            admirers=response.xpath(".//*[@class='mt-lg-5 pt-lg-2 bt-xs-1']/div[2]/a/text()")            
            admirers=admirers.extract()
            
            
            if etsysince == []:
                etsysince=''
            else:
                etsysince=etsysince[0].strip()

            if totalreviews == []:
                totalreviews=''
            else:
                totalreviews=totalreviews[0].strip()

            if shoprating == []:
                shoprating=''
            else:
                shoprating=shoprating[0].strip()

            if admirers == []:
                admirers=''
            else:
                admirers=admirers[0].strip()

            item['YrsActiveWithEtsy']=etsysince
            item['TotalShopReviews']=totalreviews
            item['ShopRating']=shoprating
            item['CountShopFavs']=admirers
                                   
            print( item)
            
           
            finalstring = item['SellerName']
            finalstring += ",\"" + item['CountShopFavs'] +"\""
            finalstring += ",\"" + item['TotalSales'] +"\""
            finalstring += ",\"" + item['TotalShopReviews'] +"\""
            finalstring += ",\"" + item['ShopRating'] +"\""
            finalstring += ",\"" + item['YrsActiveWithEtsy'] +"\""
            finalstring += ",\"" + item['SellerReturnPolicy'] +"\""
            finalstring += ",\"" + item['SellerReturnDays'] +"\""
            finalstring += ",\"" + item['ShippingTime'] +"\""
            finalstring += ",\"" + item['SellerResponse'] +"\"" + "\n"
            with open(filename, 'a') as f:
                f.write(finalstring)
            self.items.append(item)
            next_page=self.nexturl
            # If present, go to next page on musical instruments catalog, else end program
            if next_page:
                yield scrapy.Request(next_page, callback=self.parse)