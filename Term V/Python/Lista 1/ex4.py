def encrypt(text, key):
	return ''.join([chr(ord(char) ^ key) for char in text])


def decrypt(cypher, key):
	return encrypt(cypher, key)
