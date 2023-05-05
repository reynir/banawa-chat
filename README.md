# Banawá chat - Trust On First Use SSH Chat

Banawa-chat is a [MirageOS](https://mirage.io/) unikernel that acts as a special SSH server that provides a chat room for connecting clients.
Clients connect with a SSH key, and if the username has not been used before the key is then associated with that user.
This is a [trust of first use (TOFU)](https://en.wikipedia.org/wiki/Trust_on_first_use) scheme for user authentication.
The user database and chat log is kept in the memory of the unikernel, and the user interface is rendered on the server.
Users don't require any other software than an SSH client in order to use the chat room.

Banawa-chat was written in May 2023 at the 12th [MirageOS hack retreat](http://retreat.mirage.io/).
As the server was written in only a few days it suffers from a number of bugs and warts (though mostly usable).
The [awa-ssh](https://github.com/mirage/awa-ssh) was [forked](https://github.com/sorbusursina/banawa-ssh) in order to support the TOFU scheme.
During development of this application three bugs were discovered, and two of them fixed.
See https://github.com/mirage/awa-ssh/pull/55, https://github.com/mirage/awa-ssh/pull/56 and https://github.com/mirage/awa-ssh/issues/57.

Thanks a lot to [@dinosaure](https://github.com/dinosaure) who provided code that I could steal for the nice terminal user interface and helped me debug many things.
Thanks as well to [@wyn](https://github.com/wyn) who helped me test and debug RSA authentication from newer ssh clients.
