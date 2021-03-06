<?xml version="1.0" encoding="UTF-8" standalone="no"?><og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
    <name>enaviSimulatedDevices</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>enaviSimulatedDevices</path>
    <decorating>false</decorating>
    <active>false</active>
    <#list buildings as building>
    <resource>
        <name>${building.name}</name>
        <type>org.ogema.model.locations.Building</type>
        <path>enaviSimulatedDevices/${building.name}</path>
        <decorating>true</decorating>
        <active>true</active>
		<resource>
			<name>temperatureSensor</name>
			<type>org.ogema.model.sensors.TemperatureSensor</type>
			<path>enaviSimulatedDevices/${building.name}/temperatureSensor</path>
			<decorating>true</decorating>
			<active>true</active>
		</resource>
		
		<resource xsi:type="og:FloatResource">
			<name>heatCapacity</name>
			<type>org.ogema.core.model.units.ThermalEnergyCapacityResource</type>
			<path>enaviSimulatedDevices/${building.name}/heatCapacity</path>
			<decorating>false</decorating>
			<active>true</active>
			<!-- typical range for a building: 2kWh/K to 15kWh/K -->
			<value>${building.heatCapacity}</value>
			<unit>J/K</unit>
		</resource>

		<resource>
		    <name>energyDemand</name>
		    <type>org.ogema.model.smartgriddata.ThermalEnergyRequest</type>
		    <path>enaviSimulatedDevices/${building.name}/energyDemand</path>
		    <decorating>true</decorating>
		    <active>true</active>
		</resource>

		<resource xsi:type="og:ResourceList">
			<path>enaviSimulatedDevices/${building.name}/heatConnections</path>
			<type>org.ogema.core.model.ResourceList</type>
			<elementType>org.ogema.model.connections.ThermalConnection</elementType>
			<name>heatConnections</name>
			<decorating>true</decorating>
			<active>true</active>
			<resourcelink>
				<link>thermalStorages/${building.thermalStorage}/heatConnections/output</link>
				<type>org.ogema.model.connections.ThermalConnection</type>
				<name>inputStorage</name>
			</resourcelink>
		</resource>

		<resource xsi:type="og:FloatResource">
			<path>enaviSimulatedDevices/${building.name}/thermalLossFactor</path>
			<type>org.ogema.core.model.units.PowerResource</type>
			<name>thermalLossFactor</name>
			<unit>W</unit>
			<decorating>false</decorating>
			<active>true</active>
			<!-- factor 100 implies e.g. a heat loss of ca. 50kWh at 0°C outside temp and 20°C inside temp -->
			<value>${building.thermalLossFactor}</value>
		</resource>

		<resource>
		    <name>location</name>
		    <type>org.ogema.model.locations.Location</type>
		    <path>enaviSimulatedDevices/${building.name}/location</path>
		    <decorating>false</decorating>
		    <active>true</active>
		    <resourcelink>
		        <link>LocationKassel</link>
		        <type>org.ogema.model.locations.GeographicLocation</type>
		        <name>geographicLocation</name>
		    </resourcelink>
		</resource>

        <resource>
            <name>electricityConnection</name>
            <type>org.ogema.model.connections.ElectricityConnection</type>
            <path>enaviSimulatedDevices/${building.name}/electricityConnection</path>
            <decorating>true</decorating>
            <active>true</active>
            <resource>
                <name>powerSensor</name>
                <type>org.ogema.model.sensors.ElectricPowerSensor</type>
                <path>enaviSimulatedDevices/${building.name}/electricityConnection/powerSensor</path>
                <decorating>false</decorating>
                <active>true</active>
                <resource xsi:type="og:FloatResource">
                    <name>reading</name>
                    <type>org.ogema.core.model.units.PowerResource</type>
                    <path>enaviSimulatedDevices/${building.name}/electricityConnection/powerSensor/reading</path>
                    <decorating>false</decorating>
                    <active>true</active>
                    <resourcelink>
                        <link>enaviSimulatedDevices/${building.name}/electricityConnection/powerSensor/reading/forecast</link>
                        <type>org.ogema.core.model.schedule.AbsoluteSchedule</type>
                        <name>forecast</name>
                    </resourcelink>
                    <value>${building.reading}</value>
                    <unit>W</unit>
                </resource>
            </resource>
        </resource>
    </resource>
    </#list>

    <#list windplants as windplant>
    <resource>
        <name>${windplant.name}</name>
        <type>org.ogema.model.devices.generators.WindPlant</type>
        <path>enaviSimulatedDevices/${windplant.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource xsi:type="og:TimeResource">
            <name>simulationUpdateFrequency</name>
            <type>org.ogema.core.model.simple.TimeResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/simulationUpdateFrequency</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${windplant.simulationUpdateFrequency}</value>
        </resource>
        <resource xsi:type="og:StringResource">
            <name>simulationProvider</name>
            <type>org.ogema.core.model.simple.StringResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/simulationProvider</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${windplant.simulationProvider}</value>
        </resource>
        <resource>
            <name>electricityConnection</name>
            <type>org.ogema.model.connections.ElectricityConnection</type>
            <path>enaviSimulatedDevices/${windplant.name}/electricityConnection</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource>
                <name>powerSensor</name>
                <type>org.ogema.model.sensors.ElectricPowerSensor</type>
                <path>enaviSimulatedDevices/${windplant.name}/electricityConnection/powerSensor</path>
                <decorating>false</decorating>
                <active>true</active>
                <resource xsi:type="og:FloatResource">
                    <name>reading</name>
                    <type>org.ogema.core.model.units.PowerResource</type>
                    <path>enaviSimulatedDevices/${windplant.name}/electricityConnection/powerSensor/reading</path>
                    <decorating>false</decorating>
                    <active>true</active>
                    <resourcelink>
                        <link>enaviSimulatedDevices/${windplant.name}/electricityConnection/powerSensor/reading/forecast</link>
                        <type>org.ogema.core.model.schedule.AbsoluteSchedule</type>
                        <name>forecast</name>
                    </resourcelink>
                    <value>${windplant.reading}</value>
                    <unit>W</unit>
                </resource>
            </resource>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>efficiency</name>
            <type>org.ogema.core.model.simple.FloatResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/efficiency</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${windplant.efficiency}</value>
        </resource>
        <resource xsi:type="og:TimeResource">
            <name>simulationForecastUpdateFrequency</name>
            <type>org.ogema.core.model.simple.TimeResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/simulationForecastUpdateFrequency</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${windplant.simulationForecastUpdateFrequency}</value>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>cutInSpeed</name>
            <type>org.ogema.core.model.units.VelocityResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/cutInSpeed</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${windplant.cutInSpeed}</value>
            <unit>m/s</unit>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>rotorArea</name>
            <type>org.ogema.core.model.units.AreaResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/rotorArea</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${windplant.rotorArea}</value>
            <unit>m2</unit>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>cutOutSpeed</name>
            <type>org.ogema.core.model.units.VelocityResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/cutOutSpeed</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${windplant.cutOutSpeed}</value>
            <unit>m/s</unit>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>ratedSpeed</name>
            <type>org.ogema.core.model.units.VelocityResource</type>
            <path>enaviSimulatedDevices/${windplant.name}/ratedSpeed</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${windplant.ratedSpeed}</value>
            <unit>m/s</unit>
        </resource>
        <resource xsi:type="og:ResourceList">
            <name>windSensors</name>
            <type>org.ogema.core.model.ResourceList</type>
            <path>enaviSimulatedDevices/${windplant.name}/windSensors</path>
            <decorating>false</decorating>
            <active>true</active>
            <resourcelink>
                <link>kassel2012Sensors/windSensor</link>
                <type>org.ogema.model.devices.sensoractordevices.WindSensor</type>
                <name>${windplant.outsideWind}</name>
            </resourcelink>
            <elementType>org.ogema.model.devices.sensoractordevices.WindSensor</elementType>
        </resource>
    </resource>
    </#list>

    <#list pvplants as pvplant>
    <resource>
        <name>${pvplant.name}</name>
        <type>org.ogema.model.devices.generators.PVPlant</type>
        <path>enaviSimulatedDevices/${pvplant.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource xsi:type="og:TimeResource">
            <name>simulationUpdateFrequency</name>
            <type>org.ogema.core.model.simple.TimeResource</type>
            <path>enaviSimulatedDevices/${pvplant.name}/simulationUpdateFrequency</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${pvplant.simulationUpdateFrequency}</value>
        </resource>
        <resource xsi:type="og:StringResource">
            <name>simulationProvider</name>
            <type>org.ogema.core.model.simple.StringResource</type>
            <path>enaviSimulatedDevices/${pvplant.name}/simulationProvider</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${pvplant.simulationProvider}</value>
        </resource>
        <resource>
            <name>electricityConnection</name>
            <type>org.ogema.model.connections.ElectricityConnection</type>
            <path>enaviSimulatedDevices/${pvplant.name}/electricityConnection</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource>
                <name>powerSensor</name>
                <type>org.ogema.model.sensors.ElectricPowerSensor</type>
                <path>enaviSimulatedDevices/${pvplant.name}/electricityConnection/powerSensor</path>
                <decorating>false</decorating>
                <active>true</active>
                <resource xsi:type="og:FloatResource">
                    <name>reading</name>
                    <type>org.ogema.core.model.units.PowerResource</type>
                    <path>enaviSimulatedDevices/${pvplant.name}/electricityConnection/powerSensor/reading</path>
                    <decorating>false</decorating>
                    <active>true</active>
                    <resourcelink>
                        <link>enaviSimulatedDevices/${pvplant.name}/electricityConnection/powerSensor/reading/forecast</link>
                        <type>org.ogema.core.model.schedule.AbsoluteSchedule</type>
                        <name>forecast</name>
                    </resourcelink>
                    <value>${pvplant.reading}</value>
                    <unit>W</unit>
                </resource>
            </resource>
        </resource>
        <resource xsi:type="og:ResourceList">
            <name>irradiationSensors</name>
            <type>org.ogema.core.model.ResourceList</type>
            <path>enaviSimulatedDevices/${pvplant.name}/irradiationSensors</path>
            <decorating>false</decorating>
            <active>true</active>
            <resourcelink>
                <link>kassel2012Sensors/solarIrradiationSensor</link>
                <type>org.ogema.model.sensors.SolarIrradiationSensor</type>
                <name>${pvplant.outsideIrradiation}</name>
            </resourcelink>
            <elementType>org.ogema.model.sensors.SolarIrradiationSensor</elementType>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>efficiency</name>
            <type>org.ogema.core.model.simple.FloatResource</type>
            <path>enaviSimulatedDevices/${pvplant.name}/efficiency</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${pvplant.efficiency}</value>
        </resource>
        <resource xsi:type="og:TimeResource">
            <name>simulationForecastUpdateFrequency</name>
            <type>org.ogema.core.model.simple.TimeResource</type>
            <path>enaviSimulatedDevices/${pvplant.name}/simulationForecastUpdateFrequency</path>
            <decorating>true</decorating>
            <active>true</active>
            <value>${pvplant.simulationForecastUpdateFrequency}</value>
        </resource>
        <resource xsi:type="og:FloatResource">
            <name>panelArea</name>
            <type>org.ogema.core.model.units.AreaResource</type>
            <path>enaviSimulatedDevices/${pvplant.name}/panelArea</path>
            <decorating>false</decorating>
            <active>true</active>
            <value>${pvplant.panelArea}</value>
            <unit>m2</unit>
        </resource>
    </resource>
    </#list>

    <#list storageDevices as storageDevice>
    <resource>
        <name>${storageDevice.name}</name>
        <type>org.ogema.model.devices.storage.ElectricityStorage</type>
        <path>enaviSimulatedDevices/${storageDevice.name}</path>
        <decorating>true</decorating>
        <active>true</active>
        <resource>
            <name>electricityConnection</name>
            <type>org.ogema.model.connections.ElectricityConnection</type>
            <path>enaviSimulatedDevices/${storageDevice.name}/electricityConnection</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource>
                <name>powerSensor</name>
                <type>org.ogema.model.sensors.ElectricPowerSensor</type>
                <path>enaviSimulatedDevices/${storageDevice.name}/electricityConnection/powerSensor</path>
                <decorating>false</decorating>
                <active>true</active>
                <resource>
                    <name>ratedValues</name>
                    <type>org.ogema.model.ranges.PowerRange</type>
                    <path>enaviSimulatedDevices/${storageDevice.name}/electricityConnection/powerSensor/ratedValues</path>
                    <decorating>false</decorating>
                    <active>true</active>
                    <resource xsi:type="og:FloatResource">
                        <name>upperLimit</name>
                        <type>org.ogema.core.model.units.PowerResource</type>
                        <path>enaviSimulatedDevices/${storageDevice.name}/electricityConnection/powerSensor/ratedValues/upperLimit</path>
                        <decorating>false</decorating>
                        <active>true</active>
                        <value>${storageDevice.upperLimit}</value>
                        <unit>W</unit>
                    </resource>
                    <resource xsi:type="og:FloatResource">
                        <name>lowerLimit</name>
                        <type>org.ogema.core.model.units.PowerResource</type>
                        <path>enaviSimulatedDevices/${storageDevice.name}/electricityConnection/powerSensor/ratedValues/lowerLimit</path>
                        <decorating>false</decorating>
                        <active>true</active>
                        <value>${storageDevice.lowerLimit}</value>
                        <unit>W</unit>
                    </resource>
                </resource>
            </resource>
        </resource>
       <resource>
            <name>chargeSensor</name>
            <type>org.ogema.model.sensors.StateOfChargeSensor</type>
            <path>enaviSimulatedDevices/${storageDevice.name}/chargeSensor</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>reading</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>enaviSimulatedDevices/${storageDevice.name}/chargeSensor/reading</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${storageDevice.reading}</value>
            </resource>
        </resource>
        <resource>
            <name>ratedEnergy</name>
            <type>org.ogema.model.ranges.EnergyRange</type>
            <path>enaviSimulatedDevices/${storageDevice.name}/ratedEnergy</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>upperLimit</name>
                <type>org.ogema.core.model.units.EnergyResource</type>
                <path>enaviSimulatedDevices/${storageDevice.name}/ratedEnergy/upperLimit</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${storageDevice.ratedEnergy_upperLimit}</value>
                <unit>J</unit>
            </resource>
            <resource xsi:type="og:FloatResource">
                <name>lowerLimit</name>
                <type>org.ogema.core.model.units.EnergyResource</type>
                <path>enaviSimulatedDevices/${storageDevice.name}/ratedEnergy/lowerLimit</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${storageDevice.ratedEnergy_lowerLimit}</value>
                <unit>J</unit>
            </resource>
        </resource>
        <resource>
            <name>setting</name>
            <type>org.ogema.model.actors.MultiSwitch</type>
            <path>enaviSimulatedDevices/${storageDevice.name}/setting</path>
            <decorating>false</decorating>
            <active>true</active>
            <resource xsi:type="og:FloatResource">
                <name>stateControl</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>enaviSimulatedDevices/${storageDevice.name}/setting/stateControl</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${storageDevice.stateControl}</value>
            </resource>
            <resource xsi:type="og:FloatResource">
                <name>stateFeedback</name>
                <type>org.ogema.core.model.simple.FloatResource</type>
                <path>enaviSimulatedDevices/${storageDevice.name}/setting/stateFeedback</path>
                <decorating>false</decorating>
                <active>true</active>
                <value>${storageDevice.stateFeedback}</value>
            </resource>
        </resource>

    </resource>
    </#list>
    <elementType>org.ogema.model.prototypes.PhysicalElement</elementType>
</og:resource>
