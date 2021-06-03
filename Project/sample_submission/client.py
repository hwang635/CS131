"""
Sample Client that can be used to test your servers based on TA hint code
Run: python3 client.py [Name of Server You Want to Send Message to]
A input prompt will pop up.
"""
import asyncio
import argparse
import sys


server_names = {
    "Riley": 12440,
    "Jaquez": 12441,
    "Juzang": 12442,
    "Campbell": 12443,
    "Bernard": 12444
}
localhost = '127.0.0.1'


class Client:
    def __init__(self, ip='127.0.0.1', port=8888, name='client', message_max_length=1e6):
        """
        127.0.0.1 is the localhost
        port could be any port
        """
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message):
        """
        on client side send the message for echo
        """
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print('{} send: {}'.format(self.name, message))
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print('{} received: {}'.format(self.name, data.decode()))

        print('close the socket')
        # The following lines closes the stream properly
        # If there is any warning, it's due to a bug o Python 3.8: https://bugs.python.org/issue38529
        # Please ignore it
        writer.close()

    def run_until_quit(self):
        # start the loop
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                message += "\n"
                asyncio.run(self.tcp_echo_client(message))


if __name__ == '__main__':
    parser = argparse.ArgumentParser('Client Argument Parser')
    parser.add_argument('server_name', type=str,
                        help='required server name that you want to connect to input')
    args = parser.parse_args()
    if not args.server_name in server_names:
        print("Invalid Server Name {}".format(args.server_name))
        sys.exit()
    client = Client(localhost, server_names[args.server_name])
    client.run_until_quit()