/* Contient toutes les fonctions nécessaires pour ajouter un exo dans la feuille courante de la base de données */

  var xhr = null; // Variable globale qui contient la requête. Globale pour éviter deux requêtes simultanées, pour ne pas surcharger le serveur...

   function ajouteexo(id,numerocadre,punid,punisguest)
   {
   	if (xhr && xhr.readyState != 0) {
		// On doit attendre que la requête ait aboutie avant d'en envoyer une deuxième....
		return;
	}
	
	xhr=getXMLHttpRequest();  // On crée la requête
   	
   	xhr.open("GET", "lib/ajouteexofeuille.php?id=" +id+"&punid="+punid+"&punisguest="+punisguest, true);  // Requête asynchrone...
   	
   	// On modifie le texte affiché...
   	
   	document.getElementById("exo"+numerocadre).disable="true";
   	document.getElementById("exo"+numerocadre).innerHTML=" [Ajout en cours...] ";
   	
   	// On prépare la réponse quand la requête est terminée : affichage intermittent de Ajout Effectué...
   	
   	xhr.onreadystatechange = function() 
   	{
		if (xhr.readyState == 4 && (xhr.status == 200 || xhr.status == 0)) 
		{
			document.getElementById("exo"+numerocadre).innerHTML = " [Ajout effectué] ";
			xhr.abort();  // On peut mettre fin à la requête.
			setTimeout (function( )
			{
				document.getElementById("exo"+numerocadre).innerHTML= " [Ajouter à ma feuille d'exos] ";
				document.getElementById("exo"+numerocadre).disable="false";
			}, 4000);
		} 
		else if (xhr.readyState == 4)
		{
  			document.getElementById("exo"+numerocadre).disable="false";
   			document.getElementById("exo"+numerocadre).innerHTML=" [Ajouter à ma feuille d'exos] ";
   			xhr.abort();
		}
	};
   	
   	// On lance la requête.
   	
   	xhr.send(null);
   	
   }