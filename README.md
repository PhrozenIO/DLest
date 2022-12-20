# DLest

<p align="center">
<img src="Assets\banner.png"/>
</p>

Welcome to DLest, the Portable Executable Exported Functions Manager. This powerful Microsoft Windows application is specifically designed to assist developers and malware analysts with the analysis and manipulation of exported functions in Portable Executable (PE) files, particularly DLLs. With DLest, you can easily enumerate exported functions using a variety of methods, including drag and drop, opening a folder, or recursively scanning a folder with regular expression filtering to only include PE files with specific export function names.

In addition to parsing PE files stored on disk, DLest also supports the analysis of memory-loaded modules, allowing you to analyze and manipulate exported functions in real time. This makes DLest an invaluable tool for malware analysts during reverse engineering or incident response. You can even dump a reconstructed version of any module for further analysis or reuse.

This application is fully multithreaded, ensuring efficient and fast processing of even large numbers of PE files. Whether you're a developer looking to analyze and manipulate exported functions or a malware analyst in need of a reliable tool to assist with your work, DLest is sure to be a valuable addition to your toolkit. Its name, DLest, reflects its ability to "deleste" you from fastidious work, streamlining and simplifying your tasks.

---

# Highlighted Features

- Supports both x86-32 (PE) and x86-64 (PE+) bit Portable Executable Files.
- Load PE File(s) from Drag n Drop (Support UAC).
- Load PE File(s) from open dialog.
- Load PE File(s) from entire folder.
- Scan for PE File(s) with advanced controls (deep scan, recursivity and export function filtering via regex).
- Scan and parse memory mapped modules from running process.
- Google Search.
- Multi Tabs.

---

# Open File Modes.

## Open Dialog

<p align="center">
<img src="Assets\mode-opend.png"/>
</p>

The open file mode in DLest allows you to use the default Microsoft Windows open dialog to select one or more Portable Executable (PE) files to be loaded into the tool. When you select the open file mode, a standard open dialog window will appear, allowing you to browse your computer and select the PE files you want to load.

Once you have selected the desired files, they will be passed through a filtering process to ensure that they are valid PE files of the same architecture as the current DLest program. This process helps to ensure that only compatible files are loaded into the tool, reducing the risk of errors or issues during processing.

Using the open file mode, you can easily load and analyze individual PE files as needed, whether for development or malware analysis purposes. Whether you need to examine a single file or a large batch of files, the open file mode in DLest makes it easy to get started.

> This will be my last huge project for 2022 and the entire 2023 year. In 2023, I will be focused on obtaining various offensive security certifications and working on a new version of the Unprotect Project with my friend Thomas Roccia. However, this does not mean that this project or other projects will be unmaintained. I will still be available to offer bug fixes as needed.

## Open Folder

<p align="center">
<img src="Assets\mode-openfolder.png"/>
</p>

The open folder mode in DLest allows you to quickly scan a single folder and identify all of the valid Portable Executable (PE) files, particularly DLLs, within it. To use this mode, simply select the open folder option and browse to the desired folder using the standard Microsoft Windows folder selection dialog.

Once you have selected the folder, DLest will scan it and identify all of the valid DLL files within it using the same filtering process as the open file mode. This process helps to ensure that only compatible files are included in the scan, reducing the risk of errors or issues during processing.

The open folder mode is a fast and efficient way to scan a single directory and identify all of the DLL files within it.

## Scan Folder

<p align="center">
<img src="Assets\mode-scan.png"/>
</p>

The scan folder mode in DLest allows you to perform a more advanced and comprehensive scan of a folder or directory structure, locating any valid DLL files that offer exported functions. This mode allows you to recursively scan a folder and its subfolders, looking for compatible and valid PE files that offer exported functions.

One of the key features of the scan folder mode is its ability to use advanced regular expression (regex) queries to filter certain files based on the names of their exported functions. This can be especially useful if you are looking for specific functions or need to exclude certain files from the scan.

In addition to DLL files, the scan folder mode also includes the option to scan for any compatible and valid PE files, rather than just limiting the search to DLLs. This makes it a versatile and powerful tool for locating and analyzing exported functions in a variety of different types of PE files.

Whether you need to quickly locate exported functions in a single folder or perform a more comprehensive and advanced scan of a larger directory structure, the scan folder mode in DLest has you covered.

## In-Memory Modules

<p align="center">
<img src="Assets\mode-mem.png"/>
</p>

<p align="center">
<img src="Assets\mode-mem-ex.png"/>
</p>

The load from running process mode in DLest allows you to parse the Portable Executable (PE) header for exported functions directly from in-memory modules, rather than from files stored on disk. This can be a useful feature for developers and malware analysts who need to analyze exported functions in real time or who are working with memory-loaded modules that are not stored on disk.

To use the load from running process mode, you will need to select the desired process from a list of currently running processes on your system. DLest will then parse the PE header for the selected process and identify any exported functions within it.

This mode is particularly useful for analyzing and manipulating exported functions in real time, as it allows you to directly access the in-memory modules of a running process. Whether you are a developer looking to optimize the performance of your code or a malware analyst trying to understand the behavior of a malicious program, the load from running process mode in DLest is a valuable tool to have at your disposal.

### What is the "Dump Reconstructed PE Image" feature ?

The "dump reconstructed PE image" feature in DLest allows you to save a copy of any memory-mapped modules from a target process to a file for further analysis or usage. This can be a useful feature for developers and malware analysts who want to examine the inner workings of a module in more detail or who need to reuse a specific version of a module for testing or other purposes.

To use this feature, you will need to select the desired process and module from a list of currently running processes and their memory-mapped modules. DLest will then create a copy of the selected module and reconstruct it in a format that can be read by PE analyzers or other tools.

The reconstructed PE image can be saved to a file on your system for later use. This allows you to examine the module in more detail, understand its behavior, or reuse it for testing or other purposes. Whether you are a developer looking to optimize the performance of your code or a malware analyst trying to understand the behavior of a malicious program, the "dump reconstructed PE image" feature in DLest is a valuable tool to have at your disposal.

# Exported Function Filtering

<p align="center">
<img src="Assets\export-filtering.png"/>
</p>

The live exports filtering feature in DLest allows you to use regular expressions to filter the exported functions displayed in the tool in real time. This can be a useful feature for developers and malware analysts who need to quickly locate specific exported functions or who want to exclude certain functions from the list.

To use this feature, simply enter a regular expression into the designated field and click the "apply" button. DLest will then use the regular expression to filter the list of exported functions, displaying only those that match the pattern.

While this feature can be very useful, it is worth noting that it may be slower when applied to a very large number of exported functions. In such cases, it may take longer for DLest to apply the filter and update the display. However, in most cases, the live exports filtering feature is fast and efficient, making it a valuable tool for quickly locating specific exported functions or excluding unwanted ones.

# Logs

<p align="center">
<img src="Assets\logs.png"/>
</p>

The log window in DLest is a feature that displays information about any errors or issues that occur while parsing Portable Executable (PE) files or when a target file does not contain exported functions. This can be a useful feature for developers and malware analysts who need to understand why certain files are not being processed correctly or who want to troubleshoot issues with the tool.

Any time an error or issue occurs while parsing a PE file or when a target file does not contain exported functions, the relevant information will be logged in the log window. This can include details about the file in question, the nature of the error or issue, and any relevant stack trace or other diagnostic information.

By using the log window, you can quickly identify and troubleshoot any problems that may arise while using DLest. Whether you are a developer working to improve the tool or a malware analyst trying to understand the behavior of a malicious program, the log window can be a valuable resource for tracking down and resolving issues.

# Extended Libraries Informations

<p align="center">
<img src="Assets\ex-lib-infos.png"/>
</p>

The extended libraries information window in DLest is a feature that displays a list of parsed Portable Executable (PE) files from the current tab context, along with a variety of details about each file. This can be a useful feature for developers and malware analysts who need to quickly access information about the libraries they are working with.

The extended libraries information window displays a list of parsed PE files, along with the following information for each file:

Library name: The name of the library, as it appears in the file's header.

Exports count: The total number of exported functions in the library.

File size: The size of the file, in bytes.

File hashes: The MD5, SHA1, and SHA256 hashes of the file, which can be used to verify its integrity or to identify it in a database of known files.

File attributes: A list of attributes associated with the file, such as whether it is read-only or hidden.

By using the extended libraries information window, you can quickly access a wealth of information about the libraries you are working with, making it easier to understand their contents and behavior. Whether you are a developer looking to optimize the performance of your code or a malware analyst trying to understand the behavior of a malicious program, the extended libraries information window is a valuable resource to have at your disposal.

# FAQ

## Not all process are shown in process list ?

There are a few reasons why not all processes may be displayed in the process list in DLest. One of the main reasons is that the process must be of the same architecture as the current DLest process. This means that if DLest is running as a 32-bit process, it will only be able to display and analyze other 32-bit processes. Similarly, if DLest is running as a 64-bit process, it will only be able to display and analyze other 64-bit processes.

Another reason why not all processes may be displayed in the process list is if DLest is currently running with limited privileges. In such cases, DLest may not be able to see or access processes that are running with higher privileges, such as those launched with User Account Control (UAC) on Windows systems. This is a security measure designed to prevent unauthorized access to sensitive processes and data.

Overall, the process list in DLest is intended to display only those processes that are compatible with the tool and that the current user has the necessary privileges to access. By limiting the process list in this way, DLest helps to ensure the security and stability of the system while still providing users with the ability to analyze and manipulate processes as needed.

## New tab is blank after loading new files ?

If a new tab context in DLest is blank, it could mean a few different things. One possibility is that no exported functions were found in the selection of files that you are trying to analyze. This could be due to a variety of factors, such as the files being corrupted, not being valid Portable Executable (PE) files, or simply not containing any exported functions.

Another possibility is that the selection of files you are trying to analyze is not compatible with DLest. For example, if the files are not PE files or are from a different architecture than the current DLest process, they may not be able to be parsed and analyzed by the tool.

# What Next ?

In future versions of DLest, it is expected to support integration with OpenAI in order to improve the analysis and understanding of extracted information from files. This could include the use of machine learning algorithms to analyze the contents of libraries and specific API's, with the goal of better understanding their expected behavior and purpose.

Additionally, future versions of DLest are expected to include an "Unprotect" API, which will allow users to expose potentially malicious libraries or API's that may be hidden or disguised in order to evade detection. This could be a valuable tool for developers and malware analysts who are working to identify and mitigate the risks posed by malicious software.

Overall, the inclusion of OpenAI integration and the "Unprotect" API in future versions of DLest is expected to significantly enhance the tool's capabilities and make it an even more powerful and useful resource for developers and malware analysts.

# Disclaimer

🇺🇸 All source code and projects shared on this Github account by Jean-Pierre LESUEUR and his company, PHROZEN SAS, are provided "as is" without warranty of any kind, either expressed or implied. The user of this code assumes all responsibility for any issues or legal liabilities that may arise from the use, misuse, or distribution of this code. The user of this code also agrees to release Jean-Pierre LESUEUR and PHROZEN SAS from any and all liability for any damages or losses that may result from the use, misuse, or distribution of this code.

By using this code, the user agrees to indemnify and hold Jean-Pierre LESUEUR and PHROZEN SAS harmless from any and all claims, liabilities, costs, and expenses arising from the use, misuse, or distribution of this code. The user also agrees not to hold Jean-Pierre LESUEUR or PHROZEN SAS responsible for any errors or omissions in the code, and to take full responsibility for ensuring that the code meets the user's needs.

This disclaimer is subject to change without notice, and the user is responsible for checking for updates. If the user does not agree to the terms of this disclaimer, they should not use this code.

---

🇫🇷 Tout les codes sources et les projets partagés sur ce compte Github par Jean-Pierre LESUEUR et sa société, PHROZEN SAS, sont fournis "tels quels" sans aucune garantie, expresse ou implicite. L'utilisateur de ce code assume toute responsabilité pour les problèmes ou les responsabilités juridiques qui pourraient résulter de l'utilisation, de l'utilisation abusive ou de la diffusion de ce code. L'utilisateur de ce code accepte également de libérer Jean-Pierre LESUEUR et PHROZEN SAS de toute responsabilité pour tous dommages ou pertes pouvant résulter de l'utilisation, de l'utilisation abusive ou de la diffusion de ce code.

En utilisant ce code, l'utilisateur accepte de garantir et de dégager Jean-Pierre LESUEUR et PHROZEN SAS de toutes réclamations, responsabilités, coûts et dépenses résultant de l'utilisation, de l'utilisation abusive ou de la diffusion de ce code. L'utilisateur accepte également de ne pas tenir Jean-Pierre LESUEUR ou PHROZEN SAS responsable des erreurs ou omissions dans le code et de prendre l'entière responsabilité de s'assurer que le code répond aux besoins de l'utilisateur.

Cette clause de non-responsabilité est sujette à modification sans préavis et l'utilisateur est responsable de vérifier les mises à jour. Si l'utilisateur n'accepte pas les termes de cette clause de non-responsabilité, il ne doit pas utiliser ce code.

# Special Thanks

- Thomas Roccia ([@fr0gger_](https://twitter.com/fr0gger_))
- [Jam Software](https://www.jam-software.com/virtual-treeview) : TVirtualStringTree Component
- [onryldz](https://github.com/onryldz/x-superobject) : XSuperObject Lib