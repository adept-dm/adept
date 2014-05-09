package adept.ui.views

import adept.ui.AdeptUIService

trait stop {
  val stopOnClose = <script>
            var stopURI = '{ AdeptUIService.stopURI }';
            <![CDATA[
            window.onbeforeunload = function(){
              $.ajax({url: stopURI});
            }
            ]]>
          </script>

}