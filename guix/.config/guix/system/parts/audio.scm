(use-modules (gnu)
             (gnu packages pulseaudio)
             (gnu packages linux)
             (gnu packages audio)
             (lib))
(os-part (list)
         (list
          pulseaudio
          alsa-lib
          bluez-alsa
          sbc)
         (list))
