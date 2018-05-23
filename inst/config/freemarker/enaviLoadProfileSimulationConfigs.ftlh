<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>loadProfileSimulationConfigs</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>loadProfileSimulationConfigs</path>
    <decorating>false</decorating>
    <active>false</active>
    <#list loadProfiles as loadProfile>
    <resource>
        <name>${loadProfile.name}</name>
        <type>de.iwes.sim.loadprofiles.model.ProfileGeneratorConfig</type>
        <path>loadProfileSimulationConfigs/${loadProfile.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource xsi:type="og:StringResource">
            <name>profileType</name>
            <type>org.ogema.core.model.simple.StringResource</type>
            <path>loadProfileSimulationConfigs/${loadProfile.name}/profileType</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${loadProfile.profileType}</value>
        </resource>
        <resourcelink>
            <link>enaviSimulatedDevices/${loadProfile.powerSensor}/electricityConnection/powerSensor</link>
            <type>org.ogema.model.sensors.ElectricPowerSensor</type>
            <name>powerSensor</name>
        </resourcelink>
        <resource xsi:type="og:FloatResource">
            <name>annualConsumption</name>
            <type>org.ogema.core.model.units.EnergyResource</type>
            <path>loadProfileSimulationConfigs/${loadProfile.name}/annualConsumption</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${loadProfile.annualConsumption}</value>
            <unit>J</unit>
        </resource>
    </resource>
    </#list>
    <elementType>de.iwes.sim.loadprofiles.model.ProfileGeneratorConfig</elementType>
</og:resource>
