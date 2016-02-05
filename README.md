# Arwo Web Application

Arwo is a web application hosted in the Apache Tomcat web container.  It is designed to provide for easy web access to
remote servers.  Its features include:

* browse the filesystems of SSH servers
* browse the filesystems of CIFS servers
* browse the filesystems of WebDAV servers
* run SQL commands on JDBC servers
* run shell commands on SSH servers
* run shell commands on Windows machines (using DCOM)
* send emails using SMTP servers
* view recent messages on IMAP email servers
* view recent messages on POP3 email servers
* manage server connections
* store "favorite" filesystem locations and commands for later use
* schedule accesses to filesystem resources (folders / files)
* schedule execution of server commands

Arwo uses additional open-source libraries internally to interface to different server types.  For each server type to
be accessed, the appropriate library / libraries should be separately downloaded.

More information about the usage of these libraries can be found on the webapp "Usage" page
[here](https://htmlpreview.github.io/?https://github.com/greyp9/arwo/blob/master/src/main/app/resources/webapp/arwo-war/usage.html).

## Requirements

To use the webapp, a version of the Java runtime is needed, as well as a version of the Tomcat web
container.  Once these are available, the webapp should be installed into the Tomcat container.  You would then
navigate a web browser to the landing page of the webapp in the running Tomcat instance and authenticate to the
application.

[sample link](https://localhost/arwo)

Because of Tomcat classloader requirements, a standalone JAR needs to be copied into the Tomcat library folder to
provide the implementation of the webapp authentication realm.

More information is provided on the webapp "Usage" page.

## License

This software is licensed under the Apache License, version 2.

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Credits

A debt of gratitude is owed to those responsible for the development of the server access libraries.  Without their
efforts, much of the functionality of this webapp would not be possible.

| Name | Description | URL |
| --- | --- | --- |
| ganymed-ssh-2 | Ganymed SSH-2: Java based SSH-2 Protocol Implementation | https://code.google.com/p/ganymed-ssh-2/ |
| jCIFS | JCIFS is an Open Source client library that implements the CIFS/SMB networking protocol in 100% Java | http://jcifs.samba.org |
| j-interop-repackaged | j-interop repackaged as a module | http://glassfish.java.net/ |
| Sardine WEBDAV client | An easy to use webdav client for java | https://github.com/lookfirst/sardine |
| Apache HttpClient | Apache HttpComponents Client | http://hc.apache.org/httpcomponents-client |
| Apache HttpCore | Apache HttpComponents Core (blocking I/O) | http://hc.apache.org/httpcomponents-core-ga |
| Apache Commons Logging | Apache Commons Logging is a thin adapter allowing configurable bridging to other, well known logging systems. | http://commons.apache.org/proper/commons-logging/ |
| javax.mail | JavaMail API | http://www.oracle.com |

<!---  /mvn:project/mvn:name  /mvn:project/mvn:description  /mvn:project/mvn:url  --->
