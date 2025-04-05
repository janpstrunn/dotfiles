# Autogenerated config.py
#
# NOTE: config.py is intended for advanced users who are comfortable
# with manually migrating the config file on qutebrowser upgrades. If
# you prefer, you can also configure qutebrowser using the
# :set/:bind/:config-* commands without having to write a config.py
# file.
#
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(True)

import os
if os.path.exists(config.configdir / "theme.py"):
    import theme
    theme.setup(c, True)

config.set('content.local_content_can_access_remote_urls', True, 'file:///home/janpstrunn/.local/share/qutebrowser/userscripts/*')
config.set('content.local_content_can_access_file_urls', False, 'file:///home/janpstrunn/.local/share/qutebrowser/userscripts/*')
