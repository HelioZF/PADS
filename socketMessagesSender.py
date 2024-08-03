import socket
import struct
import time
import json

listaProd = []

def consultar_produto_por_codigo(codigo_barras):
    try:
        # Tenta carregar o arquivo JSON
        with open('BarcodeData.json', 'r') as file:
            data = json.load(file)
    except FileNotFoundError:
        # Se o arquivo não existir, cria um dicionário vazio
        data = {"Produtos": []}
        # Salva o dicionário no arquivo JSON
        with open('BarcodeData.json', 'w') as file:
            json.dump(data, file)

    # Procura o produto com base no código de barras
    for produto in data["Produtos"]:
        if produto["CodigoBarras"] == codigo_barras:
            # Retorna o nome do produto
            if len(listaProd) >= 2:
                listaProd.pop(0)
            listaProd.append(produto["Nome"])
            print("ListaProd:", listaProd)
            return produto["Nome"]

    # Retorna None se o código de barras não for encontrado
    return None

def receive_byte_array_message(server_socket):
    try:
        # Tamanho da mensagem esperada (128 bytes)
        message_size = 128

        print("Aguardando mensagem do CLP...")

        # Configuração do timeout para 5 segundos
        server_socket.settimeout(2.0)

        # Registro do tempo inicial
        start_time = time.time()

        # Recebe a mensagem
        message_bytes, client_address = server_socket.recvfrom(message_size)

        # Converte a sequência de bytes de volta para string
        received_message = struct.unpack(f"{message_size}s", message_bytes)[0].decode('utf-8').rstrip('\x00')

        print(f"Mensagem recebida: {received_message}")

        # Consulta o produto com base no código de barras
        codigo_barras = input("Barcode: ")
        produto_nome = consultar_produto_por_codigo(codigo_barras)

        # Registro do tempo final
        end_time = time.time()

        # Cálculo do tempo decorrido
        elapsed_time = end_time - start_time
        print(f"Tempo decorrido: {elapsed_time} segundos")
        
        # Envia uma mensagem de resposta com o nome do produto
        if produto_nome is not None:
            response_message = str(produto_nome)
            if len(listaProd) > 1:
                if elapsed_time < 3 and listaProd[0] != listaProd[1]:
                    response_message += "E"
        else:
            response_message = ""

        response_message_bytes = bytearray(response_message.ljust(128)[:128], 'utf-8')
        server_socket.sendto(response_message_bytes, client_address)
        print("Client addrs: ", client_address)
        print(f"Resposta enviada: {response_message}")

    except socket.timeout:
        # Timeout expirado, envia uma mensagem vazia
        client_address = ('192.168.1.10', 7738)
        print("Timeout expirado. Enviando mensagem vazia para o CLP.")
        response_message_bytes = bytearray(''.ljust(128)[:128], 'utf-8')
        server_socket.sendto(response_message_bytes, client_address)

    except (socket.error, OSError) as e:
        print(f"Erro durante o recebimento ou envio da mensagem: {e}")

# Configurações do servidor Python
server_ip = '192.168.1.5' 
server_port = 7738

# Configurações do servidor
server = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
server.bind((server_ip, server_port))

try:
    while True:
        # Recebe a mensagem do CLP e envia uma resposta
        receive_byte_array_message(server)
        time.sleep(0.5)  # Aguarda 0.5 segundos antes de receber a próxima mensagem

except (socket.error, OSError) as e:
    print(f"Erro durante a comunicação: {e}")

finally:
    print("Fechando a conexão.")
    server.close()
