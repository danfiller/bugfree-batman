
      real*8          waveobs(100000), fluxobs(100000)

      common /spectr/ waveobs, fluxobs


