from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import default_colors, reverse, bold, normal, default

class ElegantVagrant(ColorScheme):
    progress_bar_color = 93

    def use(self, context):
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.selected:
                attr = reverse
            else:
                attr = normal
            if context.empty or context.error:
                fg = 160
                bg = 235
            if context.border:
                fg = 248
            if context.image:
                fg = 51
            if context.video:
                fg = 87
            if context.audio:
                fg = 171
            if context.document:
                fg = 135
            if context.container:
                attr |= bold
                fg = 174
            if context.directory:
                attr |= bold
                fg = 99
            elif context.executable and not \
                    any((context.media, context.container,
                         context.fifo, context.socket)):
                attr |= bold
                fg = 226
            if context.socket:
                fg = 180
                attr |= bold
            if context.fifo or context.device:
                fg = 144
                if context.device:
                    attr |= bold
            if context.link:
                fg = 99 if context.good else 116
                bg = 234
            if context.bad:
                bg = 232
            if context.tag_marker and not context.selected:
                attr |= bold
                if fg in (174, 95):
                    fg = 248
                else:
                    fg = 174
            if not context.selected and (context.cut or context.copied):
                fg = 108
                bg = 234
            if context.main_column:
                if context.selected:
                    attr |= bold
                if context.marked:
                    attr |= bold
                    fg = 99
            if context.badinfo:
                if attr & reverse:
                    bg = 95
                else:
                    fg = 95

        elif context.in_titlebar:
            attr |= bold
            if context.hostname:
                fg = 174 if context.bad else 51
            elif context.directory:
                fg = 99
            elif context.tab:
                if context.good:
                    bg = 183
            elif context.link:
                fg = 116

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = 108
                elif context.bad:
                    fg = 174
            if context.marked:
                attr |= bold | reverse
                fg = 99
            if context.message:
                if context.bad:
                    attr |= bold
                    fg = 174
            if context.loaded:
                bg = self.progress_bar_color
            if context.vcsinfo:
                fg = 116
                attr &= ~bold
            if context.vcscommit:
                fg = 144
                attr &= ~bold

        if context.text:
            if context.highlight:
                attr |= reverse

        if context.in_taskview:
            if context.title:
                fg = 116

            if context.selected:
                attr |= reverse

            if context.loaded:
                if context.selected:
                    fg = self.progress_bar_color
                else:
                    bg = self.progress_bar_color
        return fg, bg, attr
