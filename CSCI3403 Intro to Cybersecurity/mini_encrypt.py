# This is a sample Python script.

# Press ⌃R to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.


import sys
from Crypto.PublicKey import RSA
from Crypto.Random import get_random_bytes
from Crypto.Cipher import AES, PKCS1_OAEP
if len(sys.argv) != 4:
    print("[!] Invalid number of arguments")
    print("[!] Usage: python mini_encrypt.py ['encrypt' or 'decrypt'] [key file][target file]")
    exit(1)
_filename, action, rsa_key_file, target_file = sys.argv
if action == "encrypt":
    # Encrypt and write the output to a file named message.encrypted
    data = open(target_file,"rb").read()
    file_out = open("message.encrypted", "wb")
    
    recipient_key = RSA.import_key(open(rsa_key_file).read())
    session_key = get_random_bytes(16)

    # Encrypt the session key with the public RSA key
    cipher_rsa = PKCS1_OAEP.new(recipient_key)
    enc_session_key = cipher_rsa.encrypt(session_key)

    # Encrypt the data with the AES session key
    cipher_aes = AES.new(session_key, AES.MODE_GCM)
    ciphertext, tag = cipher_aes.encrypt_and_digest(data)
    [file_out.write(x) for x in (enc_session_key, cipher_aes.nonce, tag, ciphertext)]
    file_out.close()
    
elif action == "decrypt":
    # Decrypt and print
    file_in = open(target_file, "rb")
    private_key = RSA.import_key(open(rsa_key_file).read())

    enc_session_key, nonce, tag, ciphertext = \
        [file_in.read(x) for x in (private_key.size_in_bytes(), 16, 16, -1)]

    # Decrypt the session key with the private RSA key
    cipher_rsa = PKCS1_OAEP.new(private_key)
    session_key = cipher_rsa.decrypt(enc_session_key)

    # Decrypt the data with the AES session key
    cipher_aes = AES.new(session_key, AES.MODE_GCM, nonce)
    data = cipher_aes.decrypt_and_verify(ciphertext, tag)
    print(data.decode("utf-8"))
else:
    print(f"Invalid action '{action}', choose either 'encrypt' or 'decrypt'")
