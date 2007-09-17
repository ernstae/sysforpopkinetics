rm -r mdaJar
mkdir mdaJar
  cd mdaJar
  mkdir uw
    cd uw
    mkdir rfpk
      cd rfpk
      mkdir mda
        cd mda
        mkdir nonmem
          cd nonmem
          cp /home/jiaji/MDA/uw/rfpk/mda/nonmem/*.class .
          mkdir wizard
            cd wizard
            cp /home/jiaji/MDA/uw/rfpk/mda/nonmem/wizard/*.class .
            cp /home/jiaji/MDA/uw/rfpk/mda/nonmem/wizard/*.gif .
          cd ..
          mkdir display
            cd display
            cp /home/jiaji/MDA/uw/rfpk/mda/nonmem/display/*.class .
          cd ..
          mkdir compModel
            cd compModel
            cp /home/jiaji/MDA/uw/rfpk/mda/nonmem/compModel/*.class .
            cp /home/jiaji/MDA/uw/rfpk/mda/nonmem/compModel/*.png .
          cd ..         
        cd ..
      cd ..
    cd ..
  cd ..
  mkdir org
    cd org
    mkdir netbeans
      cd netbeans
      mkdir ui
        cd ui
        mkdir wizard
          cd wizard
          cp /home/jiaji/MDA/org/netbeans/ui/wizard/*.class .
          mkdir plaf
            cd plaf
            cp /home/jiaji/MDA/org/netbeans/ui/wizard/plaf/*.class .
            mkdir basic
              cd basic
              cp /home/jiaji/MDA/org/netbeans/ui/wizard/plaf/basic/*.class .
              mkdir icons
                cd icons
                cp /home/jiaji/MDA/org/netbeans/ui/wizard/plaf/basic/icons/* .
              cd ..
              mkdir resources
                cd resources
                cp /home/jiaji/MDA/org/netbeans/ui/wizard/plaf/basic/resources/* .
              cd ..
            cd ..
          cd ..
        cd ..
      cd ..
    cd ..
  cd ..
  jar cf MDA.jar *
  jarsigner -keystore ../myKeys MDA.jar jdc
  cp MDA.jar /home/jiaji/jakarta-tomcat-5.0.19/webapps/user/MDA.jar
cd ..

