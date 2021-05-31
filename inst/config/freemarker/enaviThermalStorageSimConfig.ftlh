<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
	<name>thermalStorageSimConfigs</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>thermalStorageSimConfigs</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list tsss as tss>
	<resource>
		<name>${tss.name}</name>
		<type>de.iee.sim.thermalstorage.config.ThermalStorageSimConfig</type>
		<path>thermalStorageSimConfigs/${tss.name}</path>
		<decorating>false</decorating>
		<active>true</active>
		<resourcelink>
			<link>thermalStorages/${tss.thermalStorage}</link>
			<type>org.ogema.model.devices.storage.ThermalStorage</type>
			<name>storage</name>
		</resourcelink>
		<resourcelink>
			<link>enaviSimulatedDevices/${tss.building}/energyDemand</link>
			 <type>org.ogema.model.smartgriddata.ThermalEnergyRequest</type>
			<name>energyDemand</name>
		</resourcelink>
	</resource>
    </#list>
	<elementType>de.iee.sim.thermalstorage.config.ThermalStorageSimConfig</elementType>
</og:resource>
