import tkinter
import tkinter.filedialog
import PIL
import PIL.Image
import PIL.ImageTk
import subprocess
import shutil 
import os

# by angus findlay age 21 peace out

class HyperSplit:
    def __init__(self, root):
        self.root = root
        root.title('HyperSplit')

        self.top_bar = tkinter.Entry(root)
        self.open_button = tkinter.Button(self.top_bar, text='Select Image', command=self.select)
        self.open_button.grid(row=0, column=0)

        algorithms = {'bleed', 'pixelSort', 'bleedRgb', 'pixelSort',
                'vBleedRgb', 'vPixelSort', 'shift', 'rgbShiftElm 32'}
        self.selected_algorithm = tkinter.StringVar(root)
        self.selected_algorithm.set('bleed')
        self.dropdown = tkinter.OptionMenu(self.top_bar, self.selected_algorithm, *algorithms)
        self.dropdown.grid(row=0, column=1)

        self.effect_button = tkinter.Button(self.top_bar, text='Process!',
                command=self.effect)
        self.effect_button.grid(row=0, column=2)

        self.save_button = tkinter.Button(self.top_bar, text='Save As...', command=self.save)
        self.save_button.grid(row=0, column=3)

        self.top_bar.grid(row=0)

        self.image = tkinter.Label(root)
        self.image.grid(row=1)

    def select(self):
        self.filename = tkinter.filedialog.askopenfilename(
            initialdir='~/Desktop',
            title='Select file',
            filetypes=(('PNG','*.png'),('JPG','*.jpg'),('JPEG','*.jpeg'))
        )
        if self.filename is not None and self.filename != '':
            load = PIL.Image.open(self.filename)
            self.render = PIL.ImageTk.PhotoImage(load)
            self.image.configure(image=self.render)
            self.image.image = self.render
            self.image.grid(row=1)

    def effect(self):
        args = ('./Main', self.filename, self.selected_algorithm.get())
        popen = subprocess.Popen(args, stdout=subprocess.PIPE)
        popen.wait()
        if os.path.exists('output.png'):
            load = PIL.Image.open('output.png')
            render = PIL.ImageTk.PhotoImage(load)
            self.render = render
            self.image.image = render
            self.image.configure(image=self.render)
            self.image.grid(row=1)

    def save(self):
        self.filename = tkinter.filedialog.asksaveasfilename(
            initialdir='~/Desktop',
            title='Select file',
            defaultextension='.png'
        )
        if self.filename is not None and self.filename != '':
            if os.path.exists('output.png'):
                shutil.copyfile('output.png', self.filename)

def main():
    root = tkinter.Tk()
    my_gui = HyperSplit(root)
    root.mainloop()
    if os.path.exists('output.png'):
        os.remove('output.png')

if __name__ == '__main__':
    main()
