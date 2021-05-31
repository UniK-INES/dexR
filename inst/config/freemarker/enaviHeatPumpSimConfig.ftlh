<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    
	<name>heatPumpSims</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>heatPumpSims</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list hpsims as hpsim>
	<resource>
		<name>${hpsim.name}</name>
		<type>de.iee.sim.heatpump.config.HeatPumpSimConfig</type>
		<path>heatPumpSims/${hpsim.name}</path>
		<decorating>false</decorating>
		<active>true</active>
		<resource xsi:type="og:IntegerResource">
			<name>maxNrStartsPerHour</name>
			<type>org.ogema.core.model.simple.IntegerResource</type>
			<path>heatPumpSims/${hpsim.name}/maxNrStartsPerHour</path>
			<decorating>false</decorating>
			<active>true</active>
			<value>${hpsim.maxNrStartsPerHour}</value>
		</resource>
		<resource>
			<name>coldReservoirTemperature</name>
			<type>org.ogema.model.sensors.TemperatureSensor</type>
			<path>heatPumpSims/${hpsim.name}/coldReservoirTemperature</path>
			<decorating>false</decorating>
			<active>true</active>
			<resource xsi:type="og:FloatResource">
				<path>heatPumpSims/${hpsim.name}/coldReservoirTemperature/reading</path>
				<type>org.ogema.core.model.units.TemperatureResource</type>
				<name>reading</name>
				<unit>K</unit>
				<decorating>false</decorating>
				<active>false</active>
				<value>${hpsim.coldReservoirTemperature}</value><!-- 8°C -->
			</resource>
		</resource>	
		<resourcelink>
			<link>enaviSimulatedDevices/${hpsim.building}/energyDemand</link>
			 <type>org.ogema.model.smartgriddata.ThermalEnergyRequest</type>
			<name>energyDemand</name>
		</resourcelink>
		<resourcelink>
			<link>heatpumps/${hpsim.heatPump}</link>
			<type>org.ogema.model.devices.generators.HeatPump</type>
			<name>heatPump</name>
		</resourcelink>
		<resourcelink>
			<link>thermalStorages/${hpsim.thermalStorage}</link>
			<type>org.ogema.model.devices.storage.ThermalStorage</type>
			<name>storage</name>
		</resourcelink>
		<resourcelink>
			<link>enaviSimulatedDevices/${hpsim.building}</link>
			<type>org.ogema.model.locations.Building</type>
			<name>building</name>
		</resourcelink>
		<resourcelink>
			<link>kassel2012Sensors/temperatureSensor</link>
			<type>org.ogema.model.sensors.TemperatureSensor</type>
			<name>outsideTemperature</name>
		</resourcelink>
		<resourcelink>
			<link>enaviClients/${hpsim.client}</link>
			<type>de.iwes.enavi.util.EnaviClient</type>
			<name>client</name>
		</resourcelink>
	</resource>
    </#list>
	<elementType>de.iee.sim.heatpump.config.HeatPumpSimConfig</elementType>
</og:resource>
