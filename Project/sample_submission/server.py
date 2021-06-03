import argparse
import logging
import asyncio
import re
import aiohttp
import json
import time
import sys

# Server name to port # mappings, MUST UPDATE TO THIS YEAR'S SPEC AFTER TESTING
namePortMap = {
    "Riley": 12440,
    "Jaquez": 12441,
    "Juzang": 12442,
    "Campbell": 12443,
    "Bernard": 12444
}
neighbourServerMap = {
    "Riley": ["Jaquez", "Juzang"], # Riley talks w/ Js
    "Bernard": ["Jaquez", "Juzang", "Campbell"], # Bernard talks w/ everyone but Riley
    "Juzang": ["Campbell", "Riley", "Bernard"], # Juzang talks w/ C
    "Jaquez": ["Riley", "Bernard"],
    "Campbell": ["Juzang", "Bernard"]
}

MY_APIKEY = 'AIzaSyBl_fdx-j35nYr7vE2UWQhntTSNeqfANaU'

# Represents server obj
class Server:
    def __init__(self, name, port, ip='127.0.0.1', message_max_length=1e6):
        self.name = name
        self.port = port
        self.ip = ip
        self.message_max_length = int(message_max_length)

        # Maps client by unique key to most recent existence, loc, + message
        self.recentClient = dict()
        self.clientToLocation = dict()
        self.clientToMessage = dict()
        
        # Set up log file w/ format NameServer.log open to reading + writing, 
        logging.basicConfig(filename="{}Server.log".format(self.name), 
            filemode="w+", format="%(asctime)s %(levelname)s %(message)s", 
            level=logging.INFO)
        # First log msg
        logging.info("Starting log for {}".format(self.name))
        
    # Ret true if arg can be converted to #, false otherwise
    def validNumber(self, arg):
        try:
            float(arg)
        except ValueError:
            return False
        return True
    
    # Updates maps to keep track of client time, msg, + loc
    def updateClientMap(self, clientKey, timeNum, msg):
        self.recentClient[clientKey] = timeNum
        self.clientToMessage[clientKey] = msg
        # Parse location from msg
        words = msg.split()
        loc = words[4]
        self.clientToLocation[clientKey] = loc
        
    # Checks IAMAT has valid format: # of args, valid loc, valid time
    def checkIAMAT(self, msg):
        #print("in valid IAMAT")
        # Must have 4 args
        if len(msg) != 4:
            print("IAMAT error: wrong # of args")
            return False
        
        # Check valid ISO 6709 notation for location
        #print(msg[2])
        if msg[2][0] != '+' and msg[2][0] != '-':
            print("IAMAT error: invalid coordinates")
            return False
        loc = re.split("[+-]", msg[2])
        if len(loc) != 3:
            print("IAMAT error: wrong # of coordinates")
            return False
        if not self.validNumber(loc[1]) or not self.validNumber(loc[2]):
            print("IAMAT error: invalid coordinates")
            return False
        lat = float(loc[1])
        lon = float(loc[2])
        if lat > 90 or lon > 180:
            print("IAMAT error: lat or long out of range")
            return False
    
        # Check valid POSIX time (arg is a number)
        time = msg[3]
        if not self.validNumber(time):
            return False
        
        return True # No errors, valid IAMAT msg
    
    # Handles IAMAT msg: checks validity + creates msg based on valid or not
    def handleIAMAT(self, args):
        #print("handle IAMAT")
        # If invalid IAMAT msg, immed return w/ ? msg
        if not self.checkIAMAT(args):
            #print("invalid IAMAT")
            newMsg = "? "
            return [False, newMsg]
        
        # Otherwise valid, create msg
        timeNum = float(args[3])
        timeDiff = time.time() - timeNum
        if timeDiff > 0:
            timeStr = "+" + str(timeDiff)
        else:
            timeStr = str(timeDiff)
        newMsg = "AT {} {} {} {} {}".format(self.name, timeStr, args[1], args[2], args[3])
        
        return [True, newMsg]
    
    # Checks WHATSAT msg is valid (client exists, radius + info bound w/in range)
    def checkWHATSAT(self, msg):
        # Must have 4 args
        if len(msg) != 4:
            print("WHATSAT error: wrong # of args")
            return False
        
        # Client must already exist
        clientKey = msg[1]
        if clientKey not in self.recentClient:
            print("WHATSAT error: client does not exist")
            return False
        
        # Radius must be # less than 50 km
        radius = msg[2]
        if not self.validNumber(radius) or int(radius) < 0 or int(radius) > 50:
            print("WHATSAT error: invalid radius")
            return False
            
        # Info bound must be # less than 20 items
        infoBound = msg[3]
        if not self.validNumber(infoBound) or int(infoBound) < 0 or int(infoBound) > 20:
            print("WHATSAT error: invalid info bound")
            return False
        
        return True # No errors, valid WHATSAT
    
    # Checks + parses WHATSAT msg
    def handleWHATSAT(self, msg):
        # If invalid, immed return w/ ?
        if not self.checkWHATSAT(msg):
            newMsg = "? "
            return [False, newMsg]
        
        # Otherwise, parse msg to get radius + info bound
        rad = msg[2]
        infoBound = int(msg[3])
        return [True, rad, infoBound]
    
    # Flooding alg to send updates to other servers
    async def floodOtherServers(self, msg):
        # Try to send updates to each neighbouring server 
        for otherServer in neighbourServerMap[self.name]:
            try:
                # Get portNum from map + open connection
                portNum = namePortMap[otherServer]
                _, writer = await asyncio.open_connection('127.0.0.1', portNum)
                writer.write(msg.encode())
                await writer.drain()
                logging.info("{} connected and flooded msg {} to neighbour server {}".format(self.name, msg, otherServer))
                # Use close + wait_closed together to close stream + socket
                writer.close()
                await writer.wait_closed()
                logging.info("{} closed connection to {}".format(self.name, otherServer))
            except:
                logging.info("Flood error: {} couldn't connect to {}".format(self.name, otherServer))
                
    # Async fx to parse input
    async def handle_echo(self, reader, writer):
        # Loops inf until reader @ end
        while True:
            if reader.at_eof(): # Done, break out of inf loop
                break;
        
            data = await reader.readline()
            msg = data.decode()
            # Empty msg, do nothing + skip
            if len(msg) < 1:
                continue
            parsedMsg = msg.split()
            logging.info("{} received message {}".format(self.name, msg))
            
            # Check for IAMAT or WHATSAT msg first, check if valid + process msg
            if parsedMsg[0] == "IAMAT":
                # Get array after processing IAMAT msg [valid or not, processed message]
                processedIAMAT = self.handleIAMAT(parsedMsg)
                # Invalid, just send ? msg
                if not processedIAMAT[0]:
                    newMsg = processedIAMAT[1] + msg
                #Otherwise valid, send msg to other servers
                else:
                    newMsg = processedIAMAT[1]
                    #Call fx to update client time + msg
                    self.updateClientMap(parsedMsg[1], float(parsedMsg[3]), newMsg)
                    await self.floodOtherServers(newMsg)
            # If starts w/ WHATSAT, check if valid query + process
            elif parsedMsg[0] == "WHATSAT":
                # Gets array after processing WHATSAT msg [valid or not, radius, info]
                processedWHATSAT = self.handleWHATSAT(parsedMsg)
                # Invalid, just write ? msg
                if not processedWHATSAT[0]:
                    newMsg = processedWHATSAT[1] + msg
                # Valid, process radius + infoBound + loc to query Google API
                else:
                    radius = processedWHATSAT[1]
                    infoBound = processedWHATSAT[2]
                    # Find loc from client + loc map to query places API
                    clientKey = parsedMsg[1]
                    loc = self.clientToLocation[clientKey]
                    locationResults = await self.queryAPI(loc, radius, infoBound)
                    # Message, newline, trailing newlines removed, 2 ending newlines
                    recentMsg = self.clientToMessage[clientKey]
                    newMsg = "{}\n{}\n\n".format(recentMsg, locationResults)
            # Check for AT msg from another server
            elif parsedMsg[0] == "AT":
                # Must have 6 args to be valid
                if len(parsedMsg) != 6:
                    newMsg = "? " + msg
                else:
                    logging.info("{} received msg from another server".format(self.name))
                    newMsg = None
                    
                    # Check if new msg/client or already encountered msg/client
                    clientKey = parsedMsg[3]
                    newTime = float(parsedMsg[5])
                    if not clientKey in self.recentClient: # Not in map, new
                        logging.info("Adding and sending new client {} and msg info".format(parsedMsg[1]))
                        self.updateClientMap(clientKey, newTime, msg)
                        await self.floodOtherServers(msg)
                    # Already encountered but more recent, update info    
                    elif newTime > self.recentClient[clientKey]:
                        logging.info("Update and send client {} exiting info".format(parsedMsg[1]))
                        self.updateClientMap(clientKey, newTime, msg)
                        await self.floodOtherServers(msg)
                    # Already encountered and not recent
                    else:
                        logging.info("Already encountered and less recent, do nothing")
            else:
                newMsg = "? " + msg
                    
            #Prevent NoneType errors
            if newMsg is None:
                continue
            # Otherwise, can log + write
            logging.info("{} sent {} to client".format(self.name, newMsg.rstrip('\n')))
            writer.write(newMsg.encode())
            await writer.drain()
            
        logging.info("Close the client socket")
        writer.close()
            
    async def fetch (self, sess, url):
        async with sess.get(url) as response:
            return await response.text()
            
    # Async fx to query Google API for loc info
    async def queryAPI(self, loc, radius, infoBound):
        # https://docs.aiohttp.org/en/stable/client_quickstart.html
        async with aiohttp.ClientSession() as sess:
            # Get lat/long coord format, find index of + or - separator where the comma should go
            sep = loc.rfind("+")
            if sep == -1 or sep == 0:
                sep = loc.rfind("-")
            # Put loc in format lat,long
            commaLoc = loc[0:sep] + "," + loc[sep:]
            
            # Query API, https://stackoverflow.com/questions/2660201/what-parameters-should-i-use-in-a-google-maps-url-to-go-to-a-lat-lon
            url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}".format(commaLoc, radius, MY_APIKEY)
            queryResp = await self.fetch(sess, url)
            parsedResp = json.loads(queryResp)
            
            # Limit # of results to info bound, strip trailing newlines
            if len(parsedResp["results"]) > infoBound:
                parsedResp["results"] = parsedResp["results"][:infoBound]
            logging.info("{} queried {} locations from Google Place API".format(self.name, len(parsedResp["results"])))
            return json.dumps(parsedResp, indent=4).rstrip('\n')
        
    async def run_forever(self):
        # Start server
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        async with server:
            await server.serve_forever()
        # Close the server
        logging.info("Done, closing {}".format(self.name))
        server.close()
        
# Based off TA hint code echo_server.py, parse cmd line + init Server obj
def main():
    # Check cmd line has correct # of args before parsing
    if len(sys.argv) != 2:
        print("Parsing error: wrong # of arguments")
        sys.exit(1)

    # Create arg parser + parse cmd line args
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str, help='required server name input')
    args = parser.parse_args()
    
    #Check that passed in server name exists, if valid then start server
    serverName = args.server_name
    if not serverName in namePortMap:
        print("Parsing error: server name does not exist")
        sys.exit(1)
    
    #Init server obj
    portNum = namePortMap[serverName]
    server = Server(serverName, portNum)

    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass
    
if __name__ == '__main__':
    main()