<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>enaviWindPvRequests</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>enaviWindPvRequests</path>
    <decorating>false</decorating>
    <active>false</active>
    <#list generations as generation>
    <resource>
        <name>${generation.name}</name>
        <type>de.iwes.enavi.windpv.requests.model.PvConfiguration</type>
        <path>enaviWindPvRequests/${generation.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resourcelink>
            <link>enaviClients/${generation.client}</link>
            <type>de.iwes.enavi.util.EnaviClient</type>
            <name>client</name>
        </resourcelink>
        <resource xsi:type="og:ResourceList">
            <name>requests</name>
            <type>org.ogema.core.model.ResourceList</type>
            <path>enaviWindPvRequests/${generation.name}/requests</path>
            <decorating>true</decorating>
            <active>false</active>
            <elementType>org.ogema.model.smartgriddata.ElectricEnergyRequest</elementType>
        </resource>
        <resourcelink>
            <link>enaviSimulatedDevices/${generation.device}</link>
            <type>org.ogema.model.devices.generators.${generation.type}</type>
            <name>device</name>
        </resourcelink>
    </resource>
    </#list>
    <elementType>de.iwes.enavi.util.DeviceBasedRequestConfiguration</elementType>
</og:resource>
