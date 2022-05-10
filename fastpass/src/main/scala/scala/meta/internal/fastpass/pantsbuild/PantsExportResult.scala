package scala.meta.internal.fastpass.pantsbuild

import java.nio.file.Path

class PantsExportResult(
    val exportedTargets: Int,
    val internalSources: collection.Map[Path, Path],
    val javaHome: Option[Path],
    val libraries: Iterator[PantsLibrary]
    // val pantsExport: PantsExport
)
