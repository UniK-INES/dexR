<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>OGEMASimulationConfiguration</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>OGEMASimulationConfiguration</path>
    <decorating>false</decorating>
    <active>false</active>
   <#list ogemaConfigurations as ogemaConfig>
    <resource>
        <name>${ogemaConfig.name}</name>
        <type>de.iwes.elsim.battery.BatterySimConfig</type>
        <path>OGEMASimulationConfiguration/${ogemaConfig.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource xsi:type="og:TimeResource">
            <name>updateInterval</name>
            <type>org.ogema.core.model.simple.TimeResource</type>
            <path>OGEMASimulationConfiguration/${ogemaConfig.name}/updateInterval</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${ogemaConfig.updateInterval}</value>
        </resource>
        <resource xsi:type="og:StringResource">
            <name>simulationProviderId</name>
            <type>org.ogema.core.model.simple.StringResource</type>
            <path>OGEMASimulationConfiguration/${ogemaConfig.name}/simulationProviderId</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${ogemaConfig.simulationProviderId}</value>
        </resource>
        <resourcelink>
            <link>enaviSimulatedDevices/${ogemaConfig.simulatedDevice}</link>
            <type>org.ogema.model.devices.storage.ElectricityStorage</type>
            <name>target</name>
        </resourcelink>
    </resource>
    </#list>
    <elementType>org.ogema.tools.simulation.service.apiplus.SimulationConfigurationModel</elementType>
</og:resource>
