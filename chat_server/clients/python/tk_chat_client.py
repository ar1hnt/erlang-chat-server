import socket
import threading
import tkinter as tk
import time

from tkinter import simpledialog

HOST = '127.0.0.1'
PORT = 5555
RECONNECT_DELAY = 2.0

class ChatClient:
    def __init__(self, master):
        self.master = master
        self.master.title('Чат Erlang')
        self.text = tk.Text(master, state='disabled', wrap='word', height=20, width=80)
        self.text.pack(padx=8, pady=8)
        self.entry = tk.Entry(master, width=80)
        self.entry.pack(padx=8, pady=(0,8))
        self.entry.bind('<Return>', self.send_message)
        self.status = tk.Label(master, text='Disconnected', anchor='w')
        self.status.pack(fill='x')

        self.sock = None
        self.stop_event = threading.Event()
        self.reader_thread = None
        self.nick = simpledialog.askstring('Имя', 'Введите ваше имя:') or ''

        self.connect_async()

    def log(self, line):
        self.text.configure(state='normal')
        self.text.insert('end', line + '\n')
        self.text.see('end')
        self.text.configure(state='disabled')

    def set_status(self, s):
        self.status.config(text=s)

    def connect(self):
        while not self.stop_event.is_set():
            try:
                s = socket.create_connection((HOST, PORT), timeout=5)
                self.sock = s
                self.set_status(f'Connected to {HOST}:{PORT}')
                if self.nick:
                    self.send_line(f'/nick {self.nick}')
                return True
            except Exception as e:
                self.set_status(f'Disconnected (retry in {RECONNECT_DELAY}s)')
                time.sleep(RECONNECT_DELAY)
        return False

    def connect_async(self):
        t = threading.Thread(target=self.reader_loop, daemon=True)
        t.start()
        self.reader_thread = t

    def reader_loop(self):
        buf = ''
        while not self.stop_event.is_set():
            if self.sock is None:
                if not self.connect():
                    break
            s = None
            try:
                s = self.sock
                if s is None:
                    continue
                data = s.recv(4096)
                if not data:
                    raise ConnectionError('closed')
                text = data.decode('utf-8', errors='replace')
                buf += text
                while '\n' in buf:
                    line, buf = buf.split('\n', 1)
                    self.master.after(0, self.log, line)
            except socket.timeout:
                continue
            except Exception:
                if s:
                    try:
                        s.close()
                    except Exception:
                        pass
                self.sock = None
                self.set_status('Disconnected, reconnecting...')
                time.sleep(RECONNECT_DELAY)

    def send_line(self, s):
        if self.sock:
            try:
                self.sock.sendall((s + '\n').encode('utf-8'))
            except Exception:
                pass

    def send_message(self, event=None):
        s = self.entry.get()
        self.entry.delete(0, 'end')
        if not s:
            return
        if s.startswith('/'):
            self.send_line(s)
        else:
            self.send_line(s)

    def on_close(self):
        self.stop_event.set()
        try:
            if self.sock:
                self.sock.close()
        except Exception:
            pass
        self.master.destroy()


def main():
    root = tk.Tk()
    app = ChatClient(root)
    root.protocol('WM_DELETE_WINDOW', app.on_close)
    root.mainloop()

if __name__ == '__main__':
    main()
