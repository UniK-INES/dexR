<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>enaviClients</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>enaviClients</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list clients as client>
    <resource>
	<name>${client.name}</name>
	<type>de.iwes.enavi.util.EnaviClient</type>
	<path>enaviClients/${client.name}</path>
	<decorating>true</decorating>
	<active>true</active>
	<resourcelink>
	    <link>enaviMarkets/enavi_market</link>
	    <type>de.iwes.enavi.util.Marketplace</type>
	    <name>market</name>
	</resourcelink>
	<resource xsi:type="og:StringResource">
	    <name>clientId</name>
	    <type>org.ogema.core.model.simple.StringResource</type>
	    <path>enaviClients/${client.name}/clientId</path>
	    <decorating>false</decorating>
	    <active>true</active>
	    <value>${client.id}</value>
	</resource>
	<resource xsi:type="og:StringResource">
	    <name>pw</name>
	    <type>org.ogema.core.model.simple.StringResource</type>
	    <path>enaviClients/${client.name}/pw</path>
	    <decorating>false</decorating>
	    <active>true</active>
	    <value>${client.password}</value>
	</resource>
    </resource>
    </#list>
    <elementType>de.iwes.enavi.util.EnaviClient</elementType>
</og:resource>
