package adept.ivy

import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.repository.models.VariantMetadata
import adept.models.Artifact
import java.io.File
import adept.ext.conversions.Conversion
import org.apache.ivy.core.resolve.IvyNode
import adept.models.Id

case class IvyImportResult(mrid: ModuleRevisionId, dependencies: Map[String, Set[IvyNode]], variantMetadata: VariantMetadata, artifacts: Set[Artifact], localFiles: Set[(Artifact, File, String)]) {
 def convertWith(conversion: Conversion, others: Set[VariantMetadata]) = {
   conversion.convert(variantMetadata, others).map { oldMetadata =>
     this.copy( variantMetadata = oldMetadata )
   }
 }
}